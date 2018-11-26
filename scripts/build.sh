#!/usr/bin/env sh

set -eo pipefail

docker build . --tag reason-graphql-experiment
docker rm reason-graphql-experiment || true
docker create --name reason-graphql-experiment reason-graphql-experiment
docker cp reason-graphql-experiment:/graphql/main.exe .