#!/bin/bash

NAME=$(whoami)
NUM_OF_PROCESS=$(ps -ef | awk "/teamviewer/ && /^$NAME/ && /teamviewer\/.?tv_bin/ {print}" | wc -l)

if (( NUM_OF_PROCESS <= 0 )); then
    echo "teamviewer is not running, trying to start"
    /usr/bin/teamviewer &
    echo "teamviewer successfully started"
else
    echo "teamviewer is running, skip"
fi
