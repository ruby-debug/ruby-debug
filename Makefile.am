SUBDIRS = doc emacs
PHONY = ChangeLog test
test: check
ChangeLog:
	svn2cl --authors=svn2cl_usermap svn://rubyforge.org/var/svn/ruby-debug/trunk
	svn2cl --authors=svn2cl_usermap svn://rubyforge.org/var/svn/ruby-debug/trunk/ext -o ext/ChangeLog
	svn2cl --authors=svn2cl_usermap svn://rubyforge.org/var/svn/ruby-debug/trunk/lib -o lib/ChangeLog
