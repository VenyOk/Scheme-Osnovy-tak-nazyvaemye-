#!/bin/bash
file_path=$2
timeout=$1
start_point=$(date +%s)
PID="-1"
while [ 0 -eq 0 ]; do
	if ! ps -p $PID > /dev/null 2>&1; then
		bash $file_path 1>>$"output.log" 2>>$"errors.log"
		PID=$!
		echo "PID: $PID"
	else
		echo "Current process don't end"
	fi
	sleep $timeout
done
