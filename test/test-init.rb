#!/usr/bin/env ruby
require 'test/unit'

# begin require 'rubygems' rescue LoadError end
# require 'ruby-debug'; Debugger.init

# Test Debugger.init and setting up ruby-debug variables
class TestDebuggerInit < Test::Unit::TestCase
  @@SRC_DIR = File.dirname(__FILE__) unless 
    defined?(@@SRC_DIR)
  def test_basic
    debugger_output = 'test-init.out'
    Dir.chdir(@@SRC_DIR) do 
      ENV['EMACS'] = nil
      ENV['COLUMNS'] = '120'
      IO.popen("./gcd-dbg.rb 5 >#{debugger_output}", 'w') do |pipe|
        pipe.puts 'p Debugger::PROG_SCRIPT'
        pipe.puts 'show args'
        pipe.puts 'quit unconditionally'
      end
      lines = File.open(debugger_output).readlines
      expected = File.open('test-init.right').readlines
      assert_equal(expected, lines)
      File.delete(debugger_output) if expected == lines
    end
  end
end
