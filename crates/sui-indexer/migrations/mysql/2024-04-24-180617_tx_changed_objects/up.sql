-- Your SQL goes here
CREATE TABLE tx_changed_objects (
                                    tx_sequence_number          BIGINT       NOT NULL,
    -- Object Id in bytes.
                                    object_id                   BLOB        NOT NULL,
                                    PRIMARY KEY(object_id(255), tx_sequence_number)
);

