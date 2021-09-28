#!/bin/bash

set -e

curl -u ${CIRCLE_API_USER_TOKEN}: \
     -d build_parameters[CIRCLE_JOB]=release-publish \
     "https://circleci.com/api/v1.1/project/github/tkachuk-labs/reckless-trading-bot/tree/$1"
