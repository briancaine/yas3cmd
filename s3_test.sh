#!/bin/bash

mkdir -p _coverage/ coverage/
rm -f _coverage/*
BISECT_FILE=_coverage/bisect ./s3_test.byte -runner sequential $@
bisect-ppx-report -I src/ -I src/s3/ -I test/ -html coverage/ _coverage/*
