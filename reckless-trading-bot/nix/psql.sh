#!/bin/sh

set -x

psql -U nixbld1 -h localhost reckless-trading-bot
