# ********************************************************
# This tests annotations
# ********************************************************
set debuggertesting on
set callstyle last
set annotate 3
# Get into gcd
step 2
# "break" should trigger break annotation
break 10
# "delete" should trigger break annotation
delete 1
# p should not trigger a breakpoint annotation
p a
# "up" should trigger annotations
up
# "b" should trigger like "break"
b 14
# "display" should trigger display annotation
display 1+2
# undisplay should trigger display annotation
undisplay 1
step
quit
