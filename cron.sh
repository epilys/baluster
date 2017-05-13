#!/bin/sh

service baluster stop
/usr/bin/env sqlite3  << EOF
.open baluster.db
DELETE FROM baluster WHERE strftime('%s','now') - timestamp > 1209600;
EOF
service baluster start
