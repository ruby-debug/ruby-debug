# -*- encoding: utf-8 -*-
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'ruby-debug-base/version'

if defined? JRUBY_VERSION
  EXT_FILES = ['lib/ruby_debug.jar']
  EXT_RDOC_FILES = []
else
  EXT_FILES = [
    'ext/breakpoint.c',
    'ext/extconf.rb',
    'ext/ruby_debug.c',
    'ext/ruby_debug.h']
  EXT_RDOC_FILES = ['ext/ruby_debug.c']
end

Gem::Specification.new do |spec|
  spec.platform = "java" if defined? JRUBY_VERSION
  spec.name = "ruby-debug-base"
  spec.version = Debugger::VERSION

  spec.author = "Kent Sibilev"
  spec.email = "ksibilev@yahoo.com"
  spec.homepage = "https://github.com/ruby-debug/"
  spec.summary = "Fast Ruby debugger - core component"
  spec.description = <<-EOF
ruby-debug is a fast implementation of the standard Ruby debugger debug.rb.
It is implemented by utilizing a new Ruby C API hook. The core component
provides support that front-ends can build on. It provides breakpoint
handling, bindings for stack frames among other things.
EOF

  spec.extensions = ["ext/extconf.rb"] unless defined? JRUBY_VERSION
  spec.files = [
    'AUTHORS',
    'CHANGES',
    'LICENSE',
    'README',
    'Rakefile',
    'lib/ruby-debug-base.rb',
    'lib/ruby-debug-base/version.rb'] + EXT_FILES

  spec.add_dependency 'linecache', '~> 0.46'

  spec.add_development_dependency 'rake'
  spec.add_development_dependency 'rdoc'
  spec.add_development_dependency 'rake-compiler', '~> 0.8.1'

  spec.extra_rdoc_files = ['README'] + EXT_RDOC_FILES
end
