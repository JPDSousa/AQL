#!/usr/bin/env bash

MY_IP=$(curl v4.ifconfig.co)

export AQL_NAME="aql@$MY_IP"
make shell
