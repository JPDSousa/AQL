#!/bin/sh
set -ex
git clone https://github.com/SyncFree/antidote.git
cd antidote
git checkout master
make rel
