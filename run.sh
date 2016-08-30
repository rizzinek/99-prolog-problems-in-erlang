#!/bin/bash

PROBLEM=$1

erlc -o $PROBLEM $PROBLEM/$PROBLEM.erl
erl \
	-noshell \
	-pa ./funcTester \
	-pa ./$PROBLEM \
	-eval 'error_logger:tty(false)' \
	-run $PROBLEM p \
	-run init stop
