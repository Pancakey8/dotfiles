#!/usr/bin/env bash
ROOT="$(dirname $0)"
rsync -v --delete -aL $ROOT/links/ $ROOT/files
