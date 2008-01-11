#   Copyright (C) 2007, 2008 Rocky Bernstein rocky@gnu.org
#
#   This program is free software; you can redistribute it and/or modify it under
#   the terms of the GNU General Public License as published by the Free
#   Software Foundation; either version 2, or (at your option) any later
#   version.
#
#   This program is distributed in the hope that it will be useful, but WITHOUT ANY
#   WARRANTY; without even the implied warranty of MERCHANTABILITY or
#   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#   for more details.
#
#   You should have received a copy of the GNU General Public License along
#   with Bash; see the file COPYING.  If not, write to the Free Software
#   Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.
#$Id: Makefile.am,v 1.10 2007/04/13 00:59:33 rockyb Exp $

lisp_files = rdebug.el \
	     rdebug-breaks.el 	\
	     rdebug-dbg.el 	\
	     rdebug-core.el 	\
	     rdebug-frames.el 	\
	     rdebug-gud.el 	\
	     rdebug-help.el 	\
	     rdebug-layouts.el	\
	     rdebug-output.el 	\
             rdebug-regexp.el	\
	     rdebug-secondary.el\
	     rdebug-source.el	\
	     rdebug-track.el 	\
	     rdebug-varbuf.el   \
	     rdebug-vars.el 	\
	     rdebug-watch.el
check_DATA = test/elk-test.el	\
	test/test-cmd.el    	\
	test/test-core.el   	\
	test/test-indent.el 	\
	test/test-regexp.el
EXTRA_DIST = $(lisp_files) $(check_DATA)
ELCFILES   = rdebug.elc 		\
	     rdebug-breaks.elc 		\
	     rdebug-core.elc 		\
	     rdebug-dbg.el 		\
	     rdebug-gud.elc 		\
	     rdebug-frames.elc 		\
	     rdebug-help.elc 		\
	     rdebug-layouts.elc		\
	     rdebug-output.elc 		\
             rdebug-regexp.elc		\
	     rdebug-secondary.elc	\
	     rdebug-source.elc		\
	     rdebug-track.elc 		\
	     rdebug-varbuf.elc 		\
	     rdebug-vars.elc 		\
	     rdebug-watch.elc
if INSTALL_EMACS_LISP
lisp_LISP = $(lisp_files)
check: check-core check-gud check-indent check-regexp

check-gud: rdebug-gud.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -q -l test-cmd.el )

check-core: rdebug-core.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -q -l test-core.el )

check-indent: $(lisp_files)
	(cd $(srcdir)/test && \
	$(EMACS) -batch -q -l test-indent.el )

check-regexp: rdebug-regexp.el
	(cd $(srcdir)/test && \
	$(EMACS) -batch -q -l test-regexp.el )
endif

PHONY = test check check-core check-gud check-indent check-regexp

test: check

