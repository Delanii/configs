#!/bin/bash

SYSTEMD_LOG_DIR='/home/tomaskrulis/scheduledJobLogs/systemDJobLogs'
SYSTEMD_JOB_NAME='allReposMaintenance'
CURRENT_MONTH=$(date '+%b')

CURRENT_TIME=$(date '+%Y-%m-%d-%H:%M')
LOG_RECORD="${CURRENT_TIME} Initiated maintenance of all git repositories in my home folder"
LOG_FILE=$SYSTEMD_LOG_DIR/$SYSTEMD_JOB_NAME/$CURRENT_MONTH.txt

# Create directory for systemd jobs logging, if it doesn't already exist. And don't error if it does exist
mkdir -p $SYSTEMD_LOG_DIR/$SYSTEMD_JOB_NAME

# Write down log beginning!
echo $LOG_RECORD >> $LOG_FILE

# Execute all git maintenance tasks for all git repositories in my home folder
# write down output of the commands in the log
#
for dir in `find ~ -name .git -type d`; do
    cd $dir/..
    echo -e "Executing git maintenance in directory: $PWD" >> $LOG_FILE
    git maintenance run --task=gc --task=commit-graph --task=loose-objects --task=incremental-repack --task=pack-refs >> $LOG_FILE
done
