# ***************************************************
# This tests step, next, finish and continue
# ***************************************************
set debuggertesting on
set callstyle last
next 1
where
step a
step 2
where
n 2
step 1
where
step 3
where
finish
quit
