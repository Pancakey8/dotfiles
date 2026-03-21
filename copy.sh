#!/usr/bin/env bash
ROOT="$(dirname $0)"
rsync --delete -aL $ROOT/links/ $ROOT/files
