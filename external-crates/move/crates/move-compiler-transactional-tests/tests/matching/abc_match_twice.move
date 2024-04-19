//# init --edition development

//# publish
module 0x42::m {

    public enum ABC<T> has drop {
        A(T),
        B,
        C(T)
    }

    public fun a(): ABC<u64> {
        ABC::A(42)
    }

    public fun b(): ABC<u64> {
        ABC::B
    }

    public fun c(): ABC<u64> {
        ABC::C(42)
    }

    public fun t0(x: ABC<u64>): u64 {
        let default = 1;
        let y = match (&x) {
            ABC::C(x) => x,
            ABC::A(x) => x,
            ABC::B => &default,
        };
        let y = *y;
        let z = match (x) {
            ABC::C(x) => y + x,
            ABC::A(x) => y + x,
            ABC::B => y,
        };
        z
    }

    public fun t1(x: &ABC<u64>): u64 {
        let default = 1;
        let y = match (x) {
            ABC::C(x) => x,
            ABC::A(x) => x,
            ABC::B => &default,
        };
        let y = *y;
        let z = match (x) {
            ABC::C(x) => y + *x,
            ABC::A(x) => y + *x,
            ABC::B => y,
        };
        z
    }

    public fun t2(x: &mut ABC<u64>): u64 {
        let mut default = 1;
        let y = match (x) {
            ABC::C(x) => x,
            ABC::A(x) => x,
            ABC::B => &mut default,
        };
        let y = *y;
        let z = match (x) {
            ABC::C(x) => y + *x,
            ABC::A(x) => y + *x,
            ABC::B => y,
        };
        z
    }


}

//# run
module 0x42::main {
    use 0x42::m::{a, b, c};
    fun main() {
        let a = a().t0();
        assert!(a == 42 * 2, 1);

        let c = c().t0();
        assert!(c == 42 * 2, 2);

        let b = b().t0();
        assert!(b == 1, 3);
    }
}
