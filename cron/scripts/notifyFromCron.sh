#!/bin/bash

# For some reason, following is not working when ran from `cron`, but works from terminal

# Variable to hold path to cron job logs
CRON_LOG_DIR='scheduledJobLogs/cronJobLogs'
CRON_JOB_NAME='NotifyCron'
CURRENT_MONTH=$(date '+%b')

# Send notification to desktop
sleep 120
notify-send 'You can automate and schedule anything with cron today!'

# Write down in the log
CURRENT_TIME=$(date '+%Y-%m-%d-%H:%M')
LOG_RECORD="${CURRENT_TIME} Cron notification job executed."

# Create directory for cron jobs logging, if it doesn't already exist. And don't error if it does exist
mkdir -p ~/$CRON_LOG_DIR/$CRON_JOB_NAME

# Write down the greeting!
echo $LOG_RECORD >> ~/$CRON_LOG_DIR/$CRON_JOB_NAME/$CURRENT_MONTH.txt
