#!/usr/bin/env bash

ruby=${RUBY:-ruby}
rdebug=${RDEBUG:-${dir}/bin/rdebug}
dir=`dirname $0`
$ruby -I${dir}/ext:${dir}/lib:${dir}/cli -- $rdebug $*
exit $?
