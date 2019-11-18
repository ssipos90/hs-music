#!/bin/sh

set -o pipefail

mkfifo /var/log/cron.log


exec cron -f $@

