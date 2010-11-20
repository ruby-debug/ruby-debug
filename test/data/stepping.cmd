# ***************************************************
# This tests step, next, finish and continue
# ***************************************************
set debuggertesting on
set callstyle last
next
where
step a
set different on
step- ; step-
set diff off
where
n 2
step+
where
step 3
step+
where
next+	
# finish
quit
