# ***************************************************
# Test handling of info variables when we have 
# redefined inspect or to_s which give an error.
# ***************************************************
set debuggertesting on
continue 24
info variables
continue 28
info variables
quit
