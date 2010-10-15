#!/bin/bash
env ac_cv_func_getpgrp_void=no \
    ac_cv_func_setpgrp_void=yes \
    rb_cv_negative_time_t=no \
    ac_cv_func_memcmp_working=yes \
    rb_cv_binary_elf=no \
    ./configure \
    --host=i586-mingw32msvc \
    --target=i386-mingw32 \
    --build=i686-linux \
    --prefix=/usr/local/ruby-mingw32
make ruby rubyw && sudo make install
