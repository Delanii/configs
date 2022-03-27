#!/bin/bash

# Variable to hold path to cron job logs
SYSTEMD_LOG_DIR='/home/tomaskrulis/scheduledJobLogs/systemDJobLogs'
SYSTEMD_JOB_NAME='HelloSystemD'
CURRENT_DATE=$(date '+%Y-%m-%d')

# Create directory for cron jobs logging, if it doesn't already exist. And don't error if it does exist
mkdir -p $SYSTEMD_LOG_DIR/$SYSTEMD_JOB_NAME

# Write down the greeting!
echo "Hello from systemD!" >> $SYSTEMD_LOG_DIR/$SYSTEMD_JOB_NAME/$CURRENT_DATE.txt
