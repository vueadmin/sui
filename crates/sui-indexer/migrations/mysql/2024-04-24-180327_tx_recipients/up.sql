-- Your SQL goes here
CREATE TABLE tx_recipients (
                               tx_sequence_number          BIGINT       NOT NULL,
    -- SuiAddress in bytes.
                               recipient                   blob        NOT NULL,
                               PRIMARY KEY(recipient(255), tx_sequence_number)
);
CREATE INDEX tx_recipients_tx_sequence_number_index ON tx_recipients (tx_sequence_number ASC);

