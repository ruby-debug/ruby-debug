#!/usr/bin/env bash

dir=`dirname $0`
ruby -I${dir}/ext:${dir}/lib:${dir}/cli -- ${dir}/bin/rdebug $*
exit $?
