// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use std::{
    cell::OnceCell,
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};
use move_binary_format::file_format::{
    self, AbilitySet, CodeOffset, CodeUnit, CompiledModule, ConstantPoolIndex,
    FunctionHandleIndex, LocalIndex, MemberCount, SignatureToken, StructFieldInformation,
    StructHandleIndex, StructTypeParameter, TypeParameterIndex, Visibility,
};
use move_compiler::{
    self,
    compiled_unit::AnnotatedCompiledUnit,
    diagnostics::{FilesSourceText, MappedFiles},
    expansion::ast::{self as E, ModuleIdent_},
    naming::ast as N,
    shared::{
        program_info::{ConstantInfo, FunctionInfo, ModuleInfo, TypingProgramInfo},
        NumericalAddress,
    },
};
use move_core_types::{
    account_address::AccountAddress,
    annotated_value,
    language_storage::ModuleId as CoreModuleId,
    u256::U256,
};
use move_ir_types::{ast as ir, location::Spanned};
use move_symbol_pool::Symbol;

//**************************************************************************************************
// Types
//**************************************************************************************************

pub struct Model {
    files: MappedFiles,
    root_named_address_map: BTreeMap<Symbol, AccountAddress>,
    info: Arc<TypingProgramInfo>,
    // keeping separate in anticipation of compiled model
    compiled_units: BTreeMap<AccountAddress, BTreeMap<Symbol, AnnotatedCompiledUnit>>,
    // TODO package
    packages: BTreeMap<AccountAddress, PackageData>,
    //     compiled_units: BTreeMap<AccountAddress, BTreeMap<Symbol, AnnotatedCompiledUnit>>,
    //     module_deps: BTreeMap<ModuleId, BTreeMap<ModuleId, /* is immediate */ bool>>,
    //     // reverse mapping of module_deps
    //     module_used_by: BTreeMap<ModuleId, BTreeSet<ModuleId>>,
    //     function_immediate_deps: BTreeMap<QualifiedMemberId, BTreeSet<QualifiedMemberId>>,
    //     // reverse mapping of function_immediate_deps
    //     function_called_by: BTreeMap<QualifiedMemberId, BTreeSet<QualifiedMemberId>>,
}

pub trait TModuleId {
    fn module_id(&self) -> ModuleId;
}

pub type ModuleId = (AccountAddress, Symbol);
pub type QualifiedMemberId = (ModuleId, Symbol);

#[derive(Clone, Copy)]
pub struct Package<'a> {
    addr: AccountAddress,
    // TODO name. We likely want the package name from the root package's named address map
    model: &'a Model,
    data: &'a PackageData,
}

#[derive(Clone, Copy)]
pub struct Module<'a> {
    id: ModuleId,
    package: Package<'a>,
    data: &'a ModuleData,
}

#[derive(Clone, Copy)]
pub enum Member<'a> {
    Struct(Struct<'a>),
    Enum(Enum<'a>),
    Function(Function<'a>),
    Constant(Constant<'a>),
}

#[derive(Clone, Copy)]
pub enum Datatype<'a> {
    Struct(Struct<'a>),
    Enum(Enum<'a>),
}

#[derive(Clone, Copy)]
pub struct Struct<'a> {
    module: Module<'a>,
    data: &'a StructData,
}

#[derive(Clone, Copy)]
pub struct Enum<'a> {
    name: Symbol,
    module: Module<'a>,
    data: &'a EnumData,
}

#[derive(Clone, Copy)]
pub struct Field<'a> {
    struct_: Struct<'a>,
    data: &'a FieldData,
}

#[derive(Clone, Copy)]
pub struct Function<'a> {
    module: Module<'a>,
    data: &'a FunctionData,
}

#[derive(Clone, Copy)]
pub struct Constant<'a> {
    name: Symbol,
    module: Module<'a>,
    data: &'a ConstantData,
}

#[derive(Clone)]
pub enum Type {
    Bool,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    Address,
    Vector(Box<Type>),
    Struct(Box<QualifiedMemberId>),
    StructInstantiation(Box<(QualifiedMemberId, Vec<Type>)>),
    Reference(Box<Type>),
    MutableReference(Box<Type>),
    TypeParameter(TypeParameterIndex),
}

#[derive(Clone)]
pub enum Bytecode {
    Nop,
    Pop,
    Ret,
    BrTrue(CodeOffset),
    BrFalse(CodeOffset),
    Branch(CodeOffset),
    LdConst(ConstantPoolIndex),
    LdTrue,
    LdFalse,
    LdU8(u8),
    LdU16(u16),
    LdU32(u32),
    LdU64(u64),
    LdU128(Box<u128>),
    LdU256(Box<U256>),
    CastU8,
    CastU16,
    CastU32,
    CastU64,
    CastU128,
    CastU256,
    Add,
    Sub,
    Mul,
    Mod,
    Div,
    BitOr,
    BitAnd,
    Xor,
    Or,
    And,
    Not,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    Shl,
    Shr,
    Abort,
    CopyLoc(LocalIndex),
    MoveLoc(LocalIndex),
    StLoc(LocalIndex),
    Call(Box<QualifiedMemberId>),
    CallGeneric(Box<(QualifiedMemberId, Vec<Type>)>),
    Pack(Box<QualifiedMemberId>),
    PackGeneric(Box<(QualifiedMemberId, Vec<Type>)>),
    Unpack(Box<QualifiedMemberId>),
    UnpackGeneric(Box<(QualifiedMemberId, Vec<Type>)>),
    MutBorrowLoc(LocalIndex),
    ImmBorrowLoc(LocalIndex),
    MutBorrowField(Box<FieldRef>),
    MutBorrowFieldGeneric(Box<(FieldRef, Vec<Type>)>),
    ImmBorrowField(Box<FieldRef>),
    ImmBorrowFieldGeneric(Box<(FieldRef, Vec<Type>)>),
    ReadRef,
    WriteRef,
    FreezeRef,
    VecPack(Box<(Type, u64)>),
    VecLen(Box<Type>),
    VecImmBorrow(Box<Type>),
    VecMutBorrow(Box<Type>),
    VecPushBack(Box<Type>),
    VecPopBack(Box<Type>),
    VecUnpack(Box<(Type, u64)>),
    VecSwap(Box<Type>),
}

#[derive(Clone)]
pub struct FieldRef {
    struct_: QualifiedMemberId,
    field: MemberCount,
}

//**************************************************************************************************
// API
//**************************************************************************************************

impl Model {
    pub fn new(
        files: FilesSourceText,
        root_named_address_map: BTreeMap<Symbol, AccountAddress>,
        info: Arc<TypingProgramInfo>,
        compiled_units_vec: Vec<AnnotatedCompiledUnit>,
    ) -> anyhow::Result<Self> {
        let mut compiled_units = BTreeMap::new();
        for unit in compiled_units_vec {
            let package_name = unit.package_name();
            let loc = *unit.loc();
            let addr = unit.named_module.address.into_inner();
            let name = unit.named_module.name;
            let package = compiled_units.entry(addr).or_insert_with(BTreeMap::new);
            if let Some(prev) = package.insert(name, unit) {
                anyhow::bail!(
                    "Duplicate module {}::{}. \n\
                    One in package {} in file {}. \n\
                    And one in package {} in file {}",
                    prev.named_module.address,
                    prev.named_module.name,
                    prev.package_name()
                        .as_ref()
                        .map(|s| s.as_str())
                        .unwrap_or("UNKNOWN"),
                    files[&prev.loc().file_hash()].0,
                    package_name
                        .as_ref()
                        .map(|s| s.as_str())
                        .unwrap_or("UNKNOWN"),
                    files[&loc.file_hash()].0,
                );
            }
        }
        let root_named_address_reverse_map = root_named_address_map
            .iter()
            .map(|(n, a)| (*a, *n))
            .collect::<BTreeMap<_, _>>();
        let ident_map = info
            .modules
            .key_cloned_iter()
            .map(|(ident, _)| (ident.module_id(), ident))
            .collect::<BTreeMap<_, _>>();
        let packages = compiled_units
            .iter()
            .map(|(addr, units)| {
                let name = root_named_address_reverse_map.get(addr).copied();
                let data = PackageData::new(name, *addr, &ident_map, &info, units);
                (*addr, data)
            })
            .collect();
        let mut model = Self {
            files: MappedFiles::new(files),
            root_named_address_map,
            info,
            compiled_units,
            packages,
        };
        model.compute_dependencies();
        model.compute_function_dependencies();
        Ok(model)
    }

    pub fn maybe_package(&self, addr: &AccountAddress) -> Option<Package<'_>> {
        let data = self.packages.get(addr)?;
        Some(Package {
            addr: *addr,
            model: self,
            data,
        })
    }
    pub fn package(&self, addr: &AccountAddress) -> Package<'_> {
        self.maybe_package(addr).unwrap()
    }

    /// The name of the package corresponds to the name for the address in the root package's
    /// named address map. This is not the name of the package in the Move.toml file.
    pub fn package_by_name(&self, name: &Symbol) -> Option<Package<'_>> {
        let addr = self.root_named_address_map.get(name)?;
        self.maybe_package(addr)
    }

    pub fn maybe_module(&self, module: impl TModuleId) -> Option<Module<'_>> {
        let (addr, name) = module.module_id();
        let package = self.maybe_package(&addr)?;
        package.maybe_module(name)
    }
    pub fn module(&self, module: impl TModuleId) -> Module<'_> {
        self.maybe_module(module).unwrap()
    }

    pub fn packages(&self) -> impl Iterator<Item = Package<'_>> {
        self.packages.keys().map(|a| self.package(a))
    }

    pub fn modules(&self) -> impl Iterator<Item = Module<'_>> {
        self.packages
            .iter()
            .flat_map(move |(a, p)| p.modules.keys().map(move |m| self.module((a, m))))
    }

    pub fn files(&self) -> &MappedFiles {
        &self.files
    }
}

impl<'a> Package<'a> {
    pub fn address(&self) -> AccountAddress {
        self.addr
    }

    /// The name of the package corresponds to the name for the address in the root package's
    /// named address map. This is not the name of the package in the Move.toml file.
    pub fn name(&self) -> Option<Symbol> {
        self.data.name
    }

    pub fn model(&self) -> &'a Model {
        self.model
    }

    pub fn maybe_module(&self, name: impl Into<Symbol>) -> Option<Module<'a>> {
        let name = name.into();
        let data = self.data.modules.get(&name)?;
        Some(Module {
            id: (self.addr, name),
            package: *self,
            data,
        })
    }
    pub fn module(&self, name: impl Into<Symbol>) -> Module<'a> {
        self.maybe_module(name).unwrap()
    }

    pub fn modules(&self) -> impl Iterator<Item = Module<'a>> + '_ {
        self.data.modules.keys().map(move |name| self.module(*name))
    }
}

impl<'a> Module<'a> {
    pub fn model(&self) -> &'a Model {
        self.package.model()
    }

    pub fn package(&self) -> Package<'a> {
        self.package
    }

    pub fn maybe_struct(&self, name: impl Into<Symbol>) -> Option<Struct<'a>> {
        let name = name.into();
        let data = &self.data.structs.get(&name)?;
        Some(Struct {
            module: *self,
            data,
        })
    }
    pub fn struct_(&self, name: impl Into<Symbol>) -> Struct<'a> {
        self.maybe_struct(name).unwrap()
    }

    pub fn maybe_enum(&self, name: impl Into<Symbol>) -> Option<Enum<'a>> {
        let name = name.into();
        let data = &self.data.enums.get(&name)?;
        Some(Enum {
            name,
            module: *self,
            data,
        })
    }
    pub fn enum_(&self, name: impl Into<Symbol>) -> Enum<'a> {
        self.maybe_enum(name).unwrap()
    }

    pub fn maybe_function(&self, name: impl Into<Symbol>) -> Option<Function<'a>> {
        let name = name.into();
        let data = &self.data.functions.get(&name)?;
        Some(Function {
            module: *self,
            data,
        })
    }
    pub fn function(&self, name: impl Into<Symbol>) -> Function<'a> {
        self.maybe_function(name).unwrap()
    }

    pub fn maybe_constant(&self, name: impl Into<Symbol>) -> Option<Constant<'a>> {
        let name = name.into();
        let data = &self.data.constants.get(&name)?;
        Some(Constant {
            name,
            module: *self,
            data,
        })
    }
    pub fn constant(&self, name: impl Into<Symbol>) -> Constant<'a> {
        self.maybe_constant(name).unwrap()
    }

    pub fn member(&self, name: impl Into<Symbol>) -> Option<Member<'a>> {
        let name = name.into();
        self.maybe_struct(name)
            .map(Member::Struct)
            .or_else(|| self.maybe_enum(name).map(Member::Enum))
            .or_else(|| self.maybe_function(name).map(Member::Function))
            .or_else(|| self.maybe_constant(name).map(Member::Constant))
    }

    pub fn datatype(&self, name: impl Into<Symbol>) -> Option<Datatype<'_>> {
        let name = name.into();
        self.maybe_struct(name)
            .map(Datatype::Struct)
            .or_else(|| self.maybe_enum(name).map(Datatype::Enum))
    }

    pub fn structs(&self) -> impl Iterator<Item = Struct<'a>> + '_ {
        self.data.structs.keys().map(|name| self.struct_(*name))
    }

    pub fn enums(&self) -> impl Iterator<Item = Enum<'a>> + '_ {
        self.data.enums.keys().map(|name| self.enum_(*name))
    }

    pub fn functions(&self) -> impl Iterator<Item = Function<'a>> + '_ {
        self.data.functions.keys().map(|name| self.function(*name))
    }

    pub fn constants(&self) -> impl Iterator<Item = Constant<'a>> + '_ {
        self.data.constants.keys().map(|name| self.constant(*name))
    }

    pub fn info(&self) -> &'a ModuleInfo {
        self.model().info.modules.get(self.ident()).unwrap()
    }

    pub fn compiled(&self) -> &'a AnnotatedCompiledUnit {
        &self.model().compiled_units[&self.id.0][&self.id.1]
    }

    pub fn ident(&self) -> &'a E::ModuleIdent {
        &self.data.ident
    }

    pub fn id(&self) -> ModuleId {
        self.id
    }

    pub fn source_path(&self) -> Symbol {
        self.model()
            .files
            .name(&self.info().defined_loc.file_hash())
            .unwrap()
    }

    pub fn doc(&self) -> &str {
        todo!()
    }

    pub fn deps(&self) -> &'a BTreeMap<ModuleId, /* is immediate */ bool> {
        &self.data.deps
    }

    pub fn used_by(&self) -> &'a BTreeMap<ModuleId, /* is immediate */ bool> {
        &self.data.used_by
    }
}

impl<'a> Struct<'a> {
    pub fn name(&self) -> Symbol {
        self.data.name
    }

    pub fn model(&self) -> &'a Model {
        self.module.model()
    }

    pub fn package(&self) -> Package<'a> {
        self.module.package()
    }

    pub fn module(&self) -> Module<'a> {
        self.module
    }

    pub fn abilities(&self) -> AbilitySet {
        self.data.abilities
    }

    pub fn type_parameters(&self) -> &[StructTypeParameter] {
        &self.data.type_parameters
    }

    pub fn fields(&self) -> impl Iterator<Item = Field<'_>> {
        self.data.fields.iter().map(|field| Field { struct_: *self, data: field })
    }

    pub fn info(&self) -> &'a N::StructDefinition {
        self.module.info().structs.get_(&self.data.name).unwrap()
    }

    pub fn compiled(&self) -> &'a file_format::StructDefinition {
        self.module
            .compiled()
            .named_module
            .module
            .struct_def_at(self.data.compiled_idx)
    }

    pub fn compiled_idx(&self) -> file_format::StructDefinitionIndex {
        self.data.compiled_idx
    }

    pub fn struct_handle(&self) -> &'a file_format::StructHandle {
        self.module
            .compiled()
            .named_module
            .module
            .struct_handle_at(self.compiled().struct_handle)
    }

    pub fn doc(&self) -> &str {
        todo!()
    }

    pub fn field_doc(&self, _field: Symbol) -> &str {
        todo!()
    }
}

impl<'a> Enum<'a> {
    pub fn name(&self) -> Symbol {
        self.name
    }

    pub fn package(&self) -> Package<'a> {
        self.module.package()
    }

    pub fn model(&self) -> &'a Model {
        self.module.model()
    }

    pub fn module(&self) -> Module<'a> {
        self.module
    }

    pub fn info(&self) -> &'a N::EnumDefinition {
        self.module.info().enums.get_(&self.name).unwrap()
    }

    // pub fn compiled(&self) -> &file_format::EnumDefinition {
    //     self.compiled
    // }

    pub fn doc(&self) -> &str {
        todo!()
    }
}

impl<'a> Function<'a> {
    pub fn name(&self) -> Symbol {
        self.data.name
    }

    pub fn package(&self) -> Package<'a> {
        self.module.package()
    }

    pub fn model(&self) -> &'a Model {
        self.module.model()
    }

    pub fn module(&self) -> Module<'a> {
        self.module
    }

    pub fn visibility(&self) -> Visibility {
        self.data.visibility
    }

    pub fn is_entry(&self) -> bool {
        self.data.is_entry
    }

    pub fn parameters(&self) -> &[Type] {
        &self.data.parameters
    }

    pub fn returns(&self) -> &[Type] {
        &self.data.returns
    }

    pub fn type_parameters(&self) -> &[AbilitySet] {
        &self.data.type_parameters
    }

    pub fn locals(&self) -> Option<&[Type]> {
        self.data.code.as_ref().map(|c| &c.locals[..])
    }

    pub fn code(&self) ->  Option<&[Bytecode]> {
        self.data.code.as_ref().map(|c| &c.code[..])
    }

    pub fn info(&self) -> &'a FunctionInfo {
        self.module.info().functions.get_(&self.data.name).unwrap()
    }

    pub fn compiled(&self) -> &'a file_format::FunctionDefinition {
        self.module
            .compiled()
            .named_module
            .module
            .function_def_at(self.data.compiled_idx)
    }

    pub fn compiled_idx(&self) -> file_format::FunctionDefinitionIndex {
        self.data.compiled_idx
    }

    pub fn doc(&self) -> &str {
        todo!()
    }

    pub fn function_handle(&self) -> &'a file_format::FunctionHandle {
        self.module
            .compiled()
            .named_module
            .module
            .function_handle_at(self.compiled().function)
    }

    pub fn compiled_parameters(&self) -> &'a file_format::Signature {
        self.module
            .compiled()
            .named_module
            .module
            .signature_at(self.function_handle().parameters)
    }

    pub fn compiled_return_type(&self) -> &'a file_format::Signature {
        self.module
            .compiled()
            .named_module
            .module
            .signature_at(self.function_handle().return_)
    }

    /// Returns an iterator over the functions  called by this function.
    pub fn calls(&self) -> &'a BTreeSet<QualifiedMemberId> {
        &self.data.calls
    }

    /// Returns an iterator over the functions that call this function.
    pub fn called_by(&self) -> &'a BTreeSet<QualifiedMemberId> {
        &self.data.called_by
    }
}

impl<'a> Field<'a> {
    pub fn name(&self) -> Symbol {
        self.data.name
    }

    pub fn type_(&self) -> Type {
        self.data.type_.clone()
    }

    pub fn struct_(&self) -> Struct<'a> {
        self.struct_
    }
}

impl<'a> Constant<'a> {
    pub fn name(&self) -> Symbol {
        self.name
    }

    pub fn package(&self) -> Package<'a> {
        self.module.package()
    }

    pub fn model(&self) -> &'a Model {
        self.module.model()
    }

    pub fn module(&self) -> Module<'a> {
        self.module
    }

    pub fn info(&self) -> &'a ConstantInfo {
        self.module.info().constants.get_(&self.name).unwrap()
    }

    pub fn compiled(&self) -> &'a file_format::Constant {
        self.module
            .compiled()
            .named_module
            .module
            .constant_at(self.data.compiled_idx)
    }

    pub fn compiled_idx(&self) -> file_format::ConstantPoolIndex {
        self.data.compiled_idx
    }

    pub fn doc(&self) -> &str {
        todo!()
    }

    /// Returns the value of the constant as a `annotated_move::MoveValue`.
    /// This result will be cached and it will be deserialized only once.
    pub fn value(&self) -> &'a annotated_value::MoveValue {
        self.data.value.get_or_init(|| {
            let compiled = self.compiled();
            let constant_layout = Self::annotated_constant_layout(&compiled.type_);
            annotated_value::MoveValue::simple_deserialize(&compiled.data, &constant_layout)
                .unwrap()
        })
    }

    /// If the constant is a vector<u8>, it will rendered as a UTF8 string.
    /// If it has some other type (or if the data is not a valid UTF8 string),
    /// it will will call display on the `annotated_move::MoveValue`
    pub fn display_value(&self) -> String {
        let compiled = self.compiled();
        if matches!(&compiled.type_, SignatureToken::Vector(x) if x.as_ref() == &SignatureToken::U8)
        {
            if let Some(str) = bcs::from_bytes::<Vec<u8>>(&compiled.data)
                .ok()
                .and_then(|data| String::from_utf8(data).ok())
            {
                return format!("\"{str}\"");
            }
        }

        format!("{}", self.value())
    }

    fn annotated_constant_layout(ty: &SignatureToken) -> annotated_value::MoveTypeLayout {
        use annotated_value::MoveTypeLayout as L;
        use SignatureToken as ST;
        match ty {
            ST::Bool => L::Bool,
            ST::U8 => L::U8,
            ST::U16 => L::U16,
            ST::U32 => L::U16,
            ST::U64 => L::U64,
            ST::U128 => L::U128,
            ST::U256 => L::U16,
            ST::Address => L::Address,
            ST::Signer => L::Signer,
            ST::Vector(inner) => L::Vector(Box::new(Self::annotated_constant_layout(inner))),

            ST::Struct(_)
            | ST::StructInstantiation(_)
            | ST::Reference(_)
            | ST::MutableReference(_)
            | ST::TypeParameter(_) => unreachable!("{ty:?} is not supported in constants"),
        }
    }
}

//**************************************************************************************************
// Traits
//**************************************************************************************************

impl TModuleId for CoreModuleId {
    fn module_id(&self) -> ModuleId {
        (*self.address(), self.name().as_str().into())
    }
}

impl TModuleId for ModuleId {
    fn module_id(&self) -> ModuleId {
        *self
    }
}

impl TModuleId for (&AccountAddress, &Symbol) {
    fn module_id(&self) -> ModuleId {
        (*self.0, *self.1)
    }
}

impl TModuleId for (NumericalAddress, Symbol) {
    fn module_id(&self) -> ModuleId {
        (self.0.into_inner(), self.1)
    }
}

impl TModuleId for (&NumericalAddress, &Symbol) {
    fn module_id(&self) -> ModuleId {
        (self.0.into_inner(), *self.1)
    }
}
impl TModuleId for ModuleIdent_ {
    fn module_id(&self) -> ModuleId {
        let address = self.address.into_addr_bytes().into_inner();
        let module = self.module.0.value;
        (address, module)
    }
}

impl<T: TModuleId> TModuleId for &T {
    fn module_id(&self) -> ModuleId {
        T::module_id(*self)
    }
}

impl<T: TModuleId> TModuleId for Spanned<T> {
    fn module_id(&self) -> ModuleId {
        T::module_id(&self.value)
    }
}

//**************************************************************************************************
// Internals
//**************************************************************************************************

struct PackageData {
    // Based on the root packages named address map
    name: Option<Symbol>,
    modules: BTreeMap<Symbol, ModuleData>,
}

struct ModuleData {
    ident: E::ModuleIdent,
    structs: BTreeMap<Symbol, StructData>,
    enums: BTreeMap<Symbol, EnumData>,
    functions: BTreeMap<Symbol, FunctionData>,
    constants: BTreeMap<Symbol, ConstantData>,
    deps: BTreeMap<ModuleId, /* is immediate */ bool>,
    used_by: BTreeMap<ModuleId, /* is immediate */ bool>,
}

struct StructData {
    name: Symbol,
    abilities: AbilitySet,
    type_parameters: Vec<StructTypeParameter>,
    fields: Vec<FieldData>,

    // idx in the binary data (`CompiledModule`)
    compiled_idx: file_format::StructDefinitionIndex,
}

struct FieldData {
    name: Symbol,
    type_: Type,
}

struct EnumData {}

struct FunctionData {
    name: Symbol,
    visibility: Visibility,
    is_entry: bool,
    type_parameters: Vec<AbilitySet>,
    parameters: Vec<Type>,
    returns: Vec<Type>,
    code: Option<Code>,
    calls: BTreeSet<QualifiedMemberId>,
    // reverse mapping of function_immediate_deps
    called_by: BTreeSet<QualifiedMemberId>,
    compiled_idx: file_format::FunctionDefinitionIndex,
}

struct Code {
    locals: Vec<Type>,
    code: Vec<Bytecode>,
}

struct ConstantData {
    compiled_idx: file_format::ConstantPoolIndex,
    value: OnceCell<annotated_value::MoveValue>,
}

//**************************************************************************************************
// Construction
//**************************************************************************************************

impl Model {
    fn compute_dependencies(&mut self) {
        fn visit(
            compiled_units: &BTreeMap<AccountAddress, BTreeMap<Symbol, AnnotatedCompiledUnit>>,
            acc: &mut BTreeMap<ModuleId, BTreeMap<ModuleId, bool>>,
            id: ModuleId,
            unit: &AnnotatedCompiledUnit,
        ) {
            if acc.contains_key(&id) {
                return;
            }

            let immediate_deps = unit
                .named_module
                .module
                .immediate_dependencies()
                .into_iter()
                .map(|id| (*id.address(), Symbol::from(id.name().as_str())))
                .collect::<Vec<_>>();
            for immediate_dep in &immediate_deps {
                let unit = &compiled_units[&immediate_dep.0][&immediate_dep.1];
                visit(compiled_units, acc, *immediate_dep, unit);
            }
            let mut deps = BTreeMap::new();
            for immediate_dep in immediate_deps {
                deps.insert(immediate_dep, true);
                for transitive_dep in acc.get(&immediate_dep).unwrap().keys() {
                    if !deps.contains_key(transitive_dep) {
                        deps.insert(*transitive_dep, false);
                    }
                }
            }
            acc.insert(id, deps);
        }

        let mut module_deps = BTreeMap::new();
        for (a, units) in &self.compiled_units {
            for (m, unit) in units {
                let id = (*a, *m);
                visit(&self.compiled_units, &mut module_deps, id, unit);
            }
        }
        let mut module_used_by = module_deps
            .keys()
            .map(|id| (*id, BTreeMap::new()))
            .collect::<BTreeMap<_, _>>();
        for (id, deps) in &module_deps {
            for (dep, immediate) in deps {
                let immediate = *immediate;
                let used_by = module_used_by.get_mut(dep).unwrap();
                let is_immediate = used_by.entry(*id).or_insert(false);
                *is_immediate = *is_immediate || immediate;
            }
        }
        for (a, package) in &mut self.packages {
            for (m, data) in &mut package.modules {
                let id = (*a, *m);
                data.deps = module_deps.remove(&id).unwrap();
                data.used_by = module_used_by.remove(&id).unwrap();
            }
        }
    }

    fn compute_function_dependencies(&mut self) {
        let mut function_immediate_deps: BTreeMap<_, BTreeSet<_>> = BTreeMap::new();
        let units = self
            .compiled_units
            .iter()
            .flat_map(|(a, units)| units.iter().map(|(m, u)| ((*a, *m), u)));
        for (id, unit) in units {
            let module = &unit.named_module.module;
            for fdef in module.function_defs() {
                let fhandle = module.function_handle_at(fdef.function);
                let fname = module.identifier_at(fhandle.name);
                let qualified_id = (id, Symbol::from(fname.as_str()));
                let callees = fdef
                    .code
                    .as_ref()
                    .iter()
                    .flat_map(|c| c.code.iter())
                    .filter_map(|instr| match instr {
                        file_format::Bytecode::Call(i) => Some(*i),
                        file_format::Bytecode::CallGeneric(i) => {
                            Some(module.function_instantiation_at(*i).handle)
                        }
                        _ => None,
                    })
                    .map(|i| {
                        let callee_handle = module.function_handle_at(i);
                        let callee_module = module
                            .module_id_for_handle(module.module_handle_at(callee_handle.module))
                            .module_id();
                        let callee_name = module.identifier_at(fhandle.name);
                        (callee_module, Symbol::from(callee_name.as_str()))
                    })
                    .collect();
                function_immediate_deps.insert(qualified_id, callees);
            }
        }

        // ensure the map is populated for all functions
        let mut function_called_by = function_immediate_deps
            .values()
            .flatten()
            .map(|callee| (*callee, BTreeSet::new()))
            .collect::<BTreeMap<_, _>>();
        for (caller, callees) in &function_immediate_deps {
            for callee in callees {
                function_called_by.get_mut(callee).unwrap().insert(*caller);
            }
        }
        for (a, package) in &mut self.packages {
            for (m, data) in &mut package.modules {
                let id = (*a, *m);
                for (fname, fdata) in &mut data.functions {
                    let qualified_id = (id, *fname);
                    fdata.calls =
                        function_immediate_deps.remove(&qualified_id).unwrap_or(BTreeSet::new());
                    fdata.called_by =
                        function_called_by.remove(&qualified_id).unwrap_or(BTreeSet::new());
                }
            }
        }
    }
}

impl PackageData {
    fn new(
        name: Option<Symbol>,
        addr: AccountAddress,
        ident_map: &BTreeMap<ModuleId, E::ModuleIdent>,
        info: &TypingProgramInfo,
        units: &BTreeMap<Symbol, AnnotatedCompiledUnit>,
    ) -> Self {
        let modules = units
            .iter()
            .map(|(name, unit)| {
                let id = (addr, *name);
                let ident = ident_map.get(&id).unwrap();
                let info = info.module(ident);
                let data = ModuleData::new(id, *ident, info, unit);
                (*name, data)
            })
            .collect();
        Self { name, modules }
    }
}

impl ModuleData {
    fn new(
        id: ModuleId,
        ident: E::ModuleIdent,
        info: &ModuleInfo,
        unit: &AnnotatedCompiledUnit,
    ) -> Self {
        let structs = info
            .structs
            .iter()
            .map(|(_loc, name, _sinfo)| make_struct(*name, &id, &unit.named_module.module))
            .collect();
        let functions = info
            .functions
            .iter()
            .map(|(_loc, name, _finfo)| make_fun(*name, &id, &unit.named_module.module))
            .collect();
        let constants = info
            .constants
            .iter()
            .map(|(_loc, name, _cinfo)| {
                let name = *name;
                let cname = ir::ConstantName(name);
                let idx = *unit
                    .named_module
                    .source_map
                    .constant_map
                    .get(&cname)
                    .unwrap();
                let idx = file_format::ConstantPoolIndex(idx);
                let constant = ConstantData::new(idx);
                (name, constant)
            })
            .collect();
        Self {
            ident,
            structs,
            enums: BTreeMap::new(),
            functions,
            constants,
            deps: BTreeMap::new(),
            used_by: BTreeMap::new(),
        }
    }
}

impl StructData {
    fn new(
        name: Symbol,
        abilities: AbilitySet,
        type_parameters: Vec<StructTypeParameter>,
        fields: Vec<FieldData>,
        compiled_idx: file_format::StructDefinitionIndex,
    ) -> Self {
        Self {
            name,
            abilities,
            type_parameters,
            fields,
            compiled_idx,
        }
    }
}

impl FunctionData {
    fn new(
        name: Symbol,
        visibility: Visibility,
        is_entry: bool,
        type_parameters: Vec<AbilitySet>,
        parameters: Vec<Type>,
        returns: Vec<Type>,
        code: Option<Code>,
        compiled_idx: file_format::FunctionDefinitionIndex,
    ) -> Self {
        Self {
            name,
            visibility,
            is_entry,
            type_parameters,
            parameters,
            returns,
            code,
            compiled_idx,
            calls: BTreeSet::new(),
            called_by: BTreeSet::new(),
        }
    }
}

impl ConstantData {
    fn new(compiled_idx: file_format::ConstantPoolIndex) -> Self {
        Self {
            compiled_idx,
            value: OnceCell::new(),
        }
    }
}

fn into_type(ty: &SignatureToken, module: &CompiledModule, module_id: &ModuleId) -> Type {
    match ty {
        SignatureToken::Bool => Type::Bool,
        SignatureToken::U8 => Type::U8,
        SignatureToken::U16 => Type::U16,
        SignatureToken::U32 => Type::U32,
        SignatureToken::U64 => Type::U64,
        SignatureToken::U128 => Type::U128,
        SignatureToken::U256 => Type::U256,
        SignatureToken::Address => Type::Address,
        SignatureToken::Vector(ty) =>
            Type::Vector(Box::new(into_type(&*ty, module, module_id))),
        SignatureToken::Struct(idx) => {
            let struct_id = struct_id_from_handle(module, *idx, module_id);
            Type::Struct(Box::new(struct_id))
        }
        SignatureToken::StructInstantiation(st_inst) => {
            let (idx, tys) = &**st_inst;
            let struct_id = struct_id_from_handle(module, *idx, module_id);
            let type_params = tys
                .iter()
                .map(|ty| into_type(ty, module, module_id))
                .collect();
            Type::StructInstantiation(Box::new((struct_id, type_params)))
        }
        SignatureToken::Reference(ty) =>
            Type::Reference(Box::new(into_type(&*ty, module, module_id))),
        SignatureToken::MutableReference(ty) =>
            Type::MutableReference(Box::new(into_type(&*ty, module, module_id))),
        SignatureToken::TypeParameter(idx) => Type::TypeParameter(*idx),
        _ => unreachable!("Unexpected type: {:?}", ty),
    }
}

fn make_struct(
    name: Symbol,
    id: &ModuleId,
    compiled_module: &CompiledModule,
) -> (Symbol, StructData) {
    let (idx, struct_def) = compiled_module
        .find_struct_def_by_name(name.as_str())
        .unwrap();
    let struct_handle = compiled_module.struct_handle_at(struct_def.struct_handle);
    let fields =
        if let StructFieldInformation::Declared(fields) = &struct_def.field_information {
            fields
                .iter()
                .map(|field| {
                    let name = Symbol::from(
                        compiled_module.identifier_at(field.name).as_str()
                    );
                    let type_ = into_type(&field.signature.0, compiled_module, id);
                    FieldData {
                        name,
                        type_,
                    }
                })
                .collect()
        } else {
            vec![]
        };
    let struct_ = StructData::new(
        name,
        struct_handle.abilities,
        struct_handle.type_parameters.clone(),
        fields,
        idx,
    );
    (name, struct_)
}

fn make_fun(
    name: Symbol,
    id: &ModuleId,
    compiled_module: &CompiledModule,
) -> (Symbol, FunctionData) {
    let (idx, func_def) = compiled_module
        .find_function_def_by_name(name.as_str())
        .unwrap();
    let func_handle = compiled_module.function_handle_at(func_def.function);
    let visibility = func_def.visibility;
    let is_entry = func_def.is_entry;
    let type_parameters = func_handle.type_parameters.clone();
    let parameters = compiled_module
        .signature_at(func_handle.parameters)
        .0
        .iter()
        .map(|ty| into_type(ty, compiled_module, id))
        .collect();
    let returns = compiled_module
        .signature_at(func_handle.return_)
        .0
        .iter()
        .map(|ty| into_type(ty, compiled_module, id))
        .collect();
    let code = func_def
        .code
        .as_ref()
        .map(|code| make_code(code, compiled_module, id));
    let function = FunctionData::new(
        name,
        visibility,
        is_entry,
        type_parameters,
        parameters,
        returns,
        code,
        idx,
    );
    (name, function)
}

fn make_code(
    code: &CodeUnit,
    compiled_module: &CompiledModule,
    id: &ModuleId,
) -> Code {
    use file_format::Bytecode as BinaryBytecode;
    let locals = compiled_module
        .signature_at(code.locals)
        .0
        .iter()
        .map(|ty| into_type(ty, compiled_module, id))
        .collect();
    let bytecode = code
        .code
        .iter()
        .map(|instr| match instr {
            BinaryBytecode::Nop => Bytecode::Nop,
            BinaryBytecode::Pop => Bytecode::Pop,
            BinaryBytecode::Ret => Bytecode::Ret,
            BinaryBytecode::BrTrue(offset) => Bytecode::BrTrue(*offset),
            BinaryBytecode::BrFalse(offset) => Bytecode::BrFalse(*offset),
            BinaryBytecode::Branch(offset) => Bytecode::Branch(*offset),
            BinaryBytecode::LdConst(idx) => Bytecode::LdConst(*idx),
            BinaryBytecode::LdTrue => Bytecode::LdTrue,
            BinaryBytecode::LdFalse => Bytecode::LdFalse,
            BinaryBytecode::LdU8(v) => Bytecode::LdU8(*v),
            BinaryBytecode::LdU16(v) => Bytecode::LdU16(*v),
            BinaryBytecode::LdU32(v) => Bytecode::LdU32(*v),
            BinaryBytecode::LdU64(v) => Bytecode::LdU64(*v),
            BinaryBytecode::LdU128(v) => Bytecode::LdU128(v.clone()),
            BinaryBytecode::LdU256(v) => Bytecode::LdU256(v.clone()),
            BinaryBytecode::CastU8 => Bytecode::CastU8,
            BinaryBytecode::CastU16 => Bytecode::CastU16,
            BinaryBytecode::CastU32 => Bytecode::CastU32,
            BinaryBytecode::CastU64 => Bytecode::CastU64,
            BinaryBytecode::CastU128 => Bytecode::CastU128,
            BinaryBytecode::CastU256 => Bytecode::CastU256,
            BinaryBytecode::Add => Bytecode::Add,
            BinaryBytecode::Sub => Bytecode::Sub,
            BinaryBytecode::Mul => Bytecode::Mul,
            BinaryBytecode::Mod => Bytecode::Mod,
            BinaryBytecode::Div => Bytecode::Div,
            BinaryBytecode::BitOr => Bytecode::BitOr,
            BinaryBytecode::BitAnd => Bytecode::BitAnd,
            BinaryBytecode::Xor => Bytecode::Xor,
            BinaryBytecode::Or => Bytecode::Or,
            BinaryBytecode::And => Bytecode::And,
            BinaryBytecode::Not => Bytecode::Not,
            BinaryBytecode::Eq => Bytecode::Eq,
            BinaryBytecode::Neq => Bytecode::Neq,
            BinaryBytecode::Lt => Bytecode::Lt,
            BinaryBytecode::Gt => Bytecode::Gt,
            BinaryBytecode::Le => Bytecode::Le,
            BinaryBytecode::Ge => Bytecode::Ge,
            BinaryBytecode::Shl => Bytecode::Shl,
            BinaryBytecode::Shr => Bytecode::Shr,
            BinaryBytecode::Abort => Bytecode::Abort,
            BinaryBytecode::CopyLoc(idx) => Bytecode::CopyLoc(*idx),
            BinaryBytecode::MoveLoc(idx) => Bytecode::MoveLoc(*idx),
            BinaryBytecode::StLoc(idx) => Bytecode::StLoc(*idx),
            BinaryBytecode::Call(idx) => {
                let func = func_id_from_handle(compiled_module, *idx, id);
                Bytecode::Call(Box::new(func))
            }
            BinaryBytecode::CallGeneric(idx) => {
                let func_inst = compiled_module.function_instantiation_at(*idx);
                let func = func_id_from_handle(compiled_module, func_inst.handle, id);
                let type_params = compiled_module
                    .signature_at(func_inst.type_parameters)
                    .0
                    .iter()
                    .map(|ty| into_type(ty, compiled_module, id))
                    .collect();
                Bytecode::CallGeneric(Box::new((func, type_params)))
            }
            BinaryBytecode::Pack(idx) => {
                let handle_idx = compiled_module.struct_def_at(*idx).struct_handle;
                let struct_id = struct_id_from_handle(compiled_module, handle_idx, id);
                Bytecode::Pack(Box::new(struct_id))
            }
            BinaryBytecode::PackGeneric(idx) => {
                let struct_inst = compiled_module.struct_instantiation_at(*idx);
                let handle_idx = compiled_module.struct_def_at(struct_inst.def).struct_handle;
                let struct_id = struct_id_from_handle(compiled_module, handle_idx, id);
                let type_params = compiled_module
                    .signature_at(struct_inst.type_parameters)
                    .0
                    .iter()
                    .map(|ty| into_type(ty, compiled_module, id))
                    .collect();
                Bytecode::PackGeneric(Box::new((struct_id, type_params)))
            }
            BinaryBytecode::Unpack(idx) => {
                let handle_idx = compiled_module.struct_def_at(*idx).struct_handle;
                let struct_id = struct_id_from_handle(compiled_module, handle_idx, id);
                Bytecode::Unpack(Box::new(struct_id))
            }
            BinaryBytecode::UnpackGeneric(idx) => {
                let struct_inst = compiled_module.struct_instantiation_at(*idx);
                let handle_idx = compiled_module.struct_def_at(struct_inst.def).struct_handle;
                let struct_id = struct_id_from_handle(compiled_module, handle_idx, id);
                let type_params = compiled_module
                    .signature_at(struct_inst.type_parameters)
                    .0
                    .iter()
                    .map(|ty| into_type(ty, compiled_module, id))
                    .collect();
                Bytecode::UnpackGeneric(Box::new((struct_id, type_params)))
            }
            BinaryBytecode::MutBorrowLoc(idx) => Bytecode::MutBorrowLoc(*idx),
            BinaryBytecode::ImmBorrowLoc(idx) => Bytecode::ImmBorrowLoc(*idx),
            BinaryBytecode::MutBorrowField(idx) => {
                let field_handle = compiled_module.field_handle_at(*idx);
                let struct_handle =
                    compiled_module.struct_def_at(field_handle.owner).struct_handle;
                let struct_id = struct_id_from_handle(compiled_module, struct_handle, id);
                let field_ref = FieldRef {
                    struct_: struct_id,
                    field: field_handle.field,
                };
                Bytecode::MutBorrowField(Box::new(field_ref))
            }
            BinaryBytecode::MutBorrowFieldGeneric(idx) => {
                let field_inst = compiled_module.field_instantiation_at(*idx);
                let field_handle = compiled_module.field_handle_at(field_inst.handle);
                let struct_handle =
                    compiled_module.struct_def_at(field_handle.owner).struct_handle;
                let struct_id = struct_id_from_handle(compiled_module, struct_handle, id);
                let field_ref = FieldRef {
                    struct_: struct_id,
                    field: field_handle.field,
                };
                let type_params = compiled_module
                    .signature_at(field_inst.type_parameters)
                    .0
                    .iter()
                    .map(|ty| into_type(ty, compiled_module, id))
                    .collect();
                Bytecode::MutBorrowFieldGeneric(Box::new((field_ref, type_params)))
            }
            BinaryBytecode::ImmBorrowField(idx) => {
                let field_handle = compiled_module.field_handle_at(*idx);
                let struct_handle =
                    compiled_module.struct_def_at(field_handle.owner).struct_handle;
                let struct_id = struct_id_from_handle(compiled_module, struct_handle, id);
                let field_ref = FieldRef {
                    struct_: struct_id,
                    field: field_handle.field,
                };
                Bytecode::ImmBorrowField(Box::new(field_ref))
            }
            BinaryBytecode::ImmBorrowFieldGeneric(idx) => {
                let field_inst = compiled_module.field_instantiation_at(*idx);
                let field_handle = compiled_module.field_handle_at(field_inst.handle);
                let struct_handle =
                    compiled_module.struct_def_at(field_handle.owner).struct_handle;
                let struct_id = struct_id_from_handle(compiled_module, struct_handle, id);
                let field_ref = FieldRef {
                    struct_: struct_id,
                    field: field_handle.field,
                };
                let type_params = compiled_module
                    .signature_at(field_inst.type_parameters)
                    .0
                    .iter()
                    .map(|ty| into_type(ty, compiled_module, id))
                    .collect();
                Bytecode::ImmBorrowFieldGeneric(Box::new((field_ref, type_params)))
            }
            BinaryBytecode::ReadRef => Bytecode::ReadRef,
            BinaryBytecode::WriteRef => Bytecode::WriteRef,
            BinaryBytecode::FreezeRef => Bytecode::FreezeRef,
            BinaryBytecode::VecPack(idx, count) => {
                let token = &compiled_module.signature_at(*idx).0[0];
                let ty = into_type(token, compiled_module, id);
                Bytecode::VecPack(Box::new((ty, *count)))
            }
            BinaryBytecode::VecLen(idx) => {
                let token = &compiled_module.signature_at(*idx).0[0];
                let ty = into_type(token, compiled_module, id);
                Bytecode::VecLen(Box::new(ty))
            }
            BinaryBytecode::VecImmBorrow(idx) => {
                let token = &compiled_module.signature_at(*idx).0[0];
                let ty = into_type(token, compiled_module, id);
                Bytecode::VecImmBorrow(Box::new(ty))
            }
            BinaryBytecode::VecMutBorrow(idx) => {
                let token = &compiled_module.signature_at(*idx).0[0];
                let ty = into_type(token, compiled_module, id);
                Bytecode::VecMutBorrow(Box::new(ty))
            }
            BinaryBytecode::VecPushBack(idx) => {
                let token = &compiled_module.signature_at(*idx).0[0];
                let ty = into_type(token, compiled_module, id);
                Bytecode::VecPushBack(Box::new(ty))
            }
            BinaryBytecode::VecPopBack(idx) => {
                let token = &compiled_module.signature_at(*idx).0[0];
                let ty = into_type(token, compiled_module, id);
                Bytecode::VecPopBack(Box::new(ty))
            }
            BinaryBytecode::VecUnpack(idx, count) => {
                let token = &compiled_module.signature_at(*idx).0[0];
                let ty = into_type(token, compiled_module, id);
                Bytecode::VecUnpack(Box::new((ty, *count)))
            }
            BinaryBytecode::VecSwap(idx) => {
                let token = &compiled_module.signature_at(*idx).0[0];
                let ty = into_type(token, compiled_module, id);
                Bytecode::VecSwap(Box::new(ty))
            }
            _ => unreachable!("Unexpected bytecode: {:?}", instr),
        })
        .collect();
    Code { locals, code: bytecode }
}

fn struct_id_from_handle(
    compiled_module: &CompiledModule,
    handle_idx: StructHandleIndex,
    module_id: &ModuleId,
) -> QualifiedMemberId {
    let handle = compiled_module.struct_handle_at(handle_idx);
    let struct_name = Symbol::from(
        compiled_module
            .identifier_at(handle.name)
            .as_str(),
    );
    (*module_id, struct_name)
}

fn func_id_from_handle(
    compiled_module: &CompiledModule,
    handle_idx: FunctionHandleIndex,
    module_id: &ModuleId,
) -> QualifiedMemberId {
    let handle = compiled_module.function_handle_at(handle_idx);
    let func_name = Symbol::from(
        compiled_module
            .identifier_at(handle.name)
            .as_str(),
    );
    (*module_id, func_name)
}

