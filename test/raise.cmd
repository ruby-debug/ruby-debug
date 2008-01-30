# ********************************************************
# This tests that the debugger doesn't step into itself
# when the application doesn't terminate the right way.
# ********************************************************
set debuggertesting on
step
quit
