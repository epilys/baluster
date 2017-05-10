#!/bin/sh

/usr/bin/env sqlite3  << EOF
.open baluster.db
CREATE TABLE baluster (id INTEGER PRIMARY KEY, content TEXT NOT NULL, timestamp INTEGER NOT NULL);
EOF
