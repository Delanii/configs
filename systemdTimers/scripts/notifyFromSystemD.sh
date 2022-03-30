#!/bin/bash

# For some reason this is still not sending the notification to desktop.

# Variable to hold path to systemd job logs
SYSTEMD_LOG_DIR='/home/tomaskrulis/scheduledJobLogs/systemDJobLogs'
SYSTEMD_JOB_NAME='NotifySystemD'
CURRENT_MONTH=$(date '+%b')

# Send notification to desktop
notify-send 'You can automate and schedule anything with systemd today!'

# Write down in the log
CURRENT_TIME=$(date '+%Y-%m-%d-%H:%M')
LOG_RECORD="${CURRENT_TIME} SystemD notification job executed."

# Create directory for cron jobs logging, if it doesn't already exist. And don't error if it does exist
mkdir -p $SYSTEMD_LOG_DIR/$SYSTEMD_JOB_NAME

# Write down the greeting!
echo $LOG_RECORD >> $SYSTEMD_LOG_DIR/$SYSTEMD_JOB_NAME/$CURRENT_MONTH.txt
