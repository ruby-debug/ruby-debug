# -*- encoding: utf-8 -*-
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'ruby-debug-base/version'

Gem::Specification.new do |spec|
  spec.name = "ruby-debug"
  spec.version = Debugger::VERSION

  spec.author = "Kent Sibilev"
  spec.email = "ksibilev@yahoo.com"
  spec.homepage = "https://github.com/ruby-debug/"
  spec.summary = "Command line interface (CLI) for ruby-debug-base"
  spec.description = <<-EOF
A generic command line interface for ruby-debug.
EOF

  spec.require_path = "cli"
  spec.executables = ["rdebug"]
  spec.files = Dir[
    'AUTHORS',
    'CHANGES',
    'LICENSE',
    'README',
    'Rakefile',
    'cli/**/*',
    'ChangeLog',
    'bin/*',
    'doc/rdebug.1',
    'rdbg.rb']

  spec.add_dependency 'columnize', '>= 0.1'
  spec.add_dependency 'linecache', '1.3.1.pre'
  spec.add_dependency 'ruby-debug-base', "~> #{Debugger::VERSION}.0"

  spec.extra_rdoc_files = ['README']
end
