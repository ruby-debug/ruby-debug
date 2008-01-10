# ********************************************************
# This tests primarily the condition command.
# In order to do this we need to run break, and disable
# ********************************************************
set debuggertesting on
set callstyle last
set autoeval off
break 6 if a > b
info break 
condition 1
info break 
break 12
condition 2 1 == a
# FIXME: should be able to catch error on:
# condition 2 if 1 == a
disable 1
continue
info break
p a
quit
