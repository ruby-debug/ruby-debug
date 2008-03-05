#!/usr/bin/env bash

ruby=${RUBY:-ruby1.9}
dir=`dirname $0`
rdebug=debug19
$ruby -I${dir}/lib:${dir}/cli -d -w -r$rdebug $*
exit $?
