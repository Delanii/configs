#!/bin/bash

# Variable to hold path to cron job logs
CRON_LOG_DIR='cronJobLogs'
CRON_JOB_NAME='HelloCron'
CURRENT_DATE=$(date '+%Y-%m-%d')

# Create directry for cron jobs logging, if it doesn't already exist. And don't error if it does exist
mkdir -p ~/$CRON_LOG_DIR/$CRON_JOB_NAME

# Write down the greeting!
echo "Hello from cron!" >> ~/$CRON_LOG_DIR/$CRON_JOB_NAME/$CURRENT_DATE.txt
