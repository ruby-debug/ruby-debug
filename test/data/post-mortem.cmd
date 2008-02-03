# ***************************************************
# This tests the quit.
# ***************************************************
set debuggertesting on
continue
# Should have got a divide by 0 error
info program
where
up
p x
help
quit

