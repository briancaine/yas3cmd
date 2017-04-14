#!/bin/bash

rm -f _coverage/*
BISECT_FILE=_coverage/bisect ./s3_test.byte $@
bisect-ppx-report -I src/ -I src/s3/ -I test/ -html coverage/ _coverage/*
