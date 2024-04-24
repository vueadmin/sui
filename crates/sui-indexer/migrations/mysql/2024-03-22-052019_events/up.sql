CREATE TABLE events (
                               tx_sequence_number bigint NOT NULL,
                               event_sequence_number bigint NOT NULL,
                               transaction_digest BLOB NOT NULL,
                               checkpoint_sequence_number bigint NOT NULL,
                               senders JSON NOT NULL,
                               package BLOB NOT NULL,
                               module text NOT NULL,
                               event_type text NOT NULL,
                               timestamp_ms bigint NOT NULL,
                               bcs BLOB NOT NULL,
                               PRIMARY KEY(tx_sequence_number, event_sequence_number)
);

CREATE INDEX events_package ON events (package(255), tx_sequence_number, event_sequence_number);
CREATE INDEX events_package_module ON events (package(255), module(255), tx_sequence_number, event_sequence_number);
CREATE INDEX events_event_type ON events (event_type(255), tx_sequence_number, event_sequence_number);
CREATE INDEX events_checkpoint_sequence_number ON events (checkpoint_sequence_number);