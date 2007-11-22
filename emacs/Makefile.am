#   Copyright (C) 2007 Rocky Bernstein rocky@gnu.org
#
#   Bash is free software; you can redistribute it and/or modify it under
#   the terms of the GNU General Public License as published by the Free
#   Software Foundation; either version 2, or (at your option) any later
#   version.
#
#   Bash is distributed in the hope that it will be useful, but WITHOUT ANY
#   WARRANTY; without even the implied warranty of MERCHANTABILITY or
#   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#   for more details.
#   
#   You should have received a copy of the GNU General Public License along
#   with Bash; see the file COPYING.  If not, write to the Free Software
#   Foundation, 59 Temple Place, Suite 330, Boston, MA 02111 USA.
#$Id: Makefile.am,v 1.10 2007/04/13 00:59:33 rockyb Exp $

check_DATA = rdebug-test.el elk-test.el
EXTRA_DIST = rdebug.el $(check_DATA)
ELCFILES   = rdebug.elc
if INSTALL_EMACS_LISP
lisp_LISP = rdebug.el 
endif

test: check

check:
	$(EMACS) -batch -q -l $(top_builddir)/emacs/rdebug-test.el $(srcdir)/elk-test.el

