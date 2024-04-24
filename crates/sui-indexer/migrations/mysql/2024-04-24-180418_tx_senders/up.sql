-- Your SQL goes here
CREATE TABLE tx_senders (
                            tx_sequence_number          BIGINT       NOT NULL,
    -- SuiAddress in bytes.
                            sender                      BLOB        NOT NULL,
                            PRIMARY KEY(sender(255), tx_sequence_number)
);
CREATE INDEX tx_senders_tx_sequence_number_index ON tx_senders (tx_sequence_number ASC);
