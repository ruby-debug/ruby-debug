# $Id$
# This tests the functioning of some debugger command a
# parsing and set/show processing
set basename on
### *******************************
### ***   Set/show commands     ***
### *******************************
########################################
###   test args and baseneme...
########################################
set args this is a test
show args
show basename
set basename foo
show base
set basename off
show basename
set basename 0
show basename
set basename 1
show basename
########################################
###   test listsize tests...
########################################
show listsize
show listsi
set listsize abc
set listsize -20
########################################
###  test linetrace...
########################################
set linetrace on
show linetrace
set linetrace off
show linetrace
