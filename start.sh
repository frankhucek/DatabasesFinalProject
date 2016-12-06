#!/usr/bin/env bash
# [[ "root" = "$(whoami)" ]] || exit
if [ -z "$(pgrep DatabasesFinalProject-exe)" ]
then
    (cd /home/frank/Code/DatabasesFinalProject/; nohup ./DB_Site &> /dev/null &)
    echo "Website Started..."
else
    echo "Website already running"
fi
