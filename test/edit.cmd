# ********************************************************
# This tests the edit command
# ********************************************************
set debuggertesting on
# Edit using current line position.
edit
edit edit.cmd:5
# File should not exist
edit foo
# Add space to the end of 'edit'
edit 
quit
