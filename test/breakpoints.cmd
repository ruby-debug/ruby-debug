# ***************************************************
# This tests step, next, finish and continue
# ***************************************************
set debuggertesting on
set callstyle last
break 6
continue
where
break main.gcd
info break
continue
where
c 10
info break
break foo
info break
delete 1
info break
# finish
quit
