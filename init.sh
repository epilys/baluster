#!/bin/sh

/usr/bin/env sqlite3  << EOF
.open baluster.db
CREATE TABLE baluster (id VARCHAR(20) PRIMARY KEY, content BLOB NOT NULL, timestamp INTEGER NOT NULL);
EOF
