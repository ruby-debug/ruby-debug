#!/usr/bin/env ruby
require 'test/unit'

# require 'rubygems'
# require 'ruby-debug'; Debugger.init

SRC_DIR = File.dirname(__FILE__) unless defined?(SRC_DIR)

require File.join(SRC_DIR, 'helper.rb')

include TestHelper

# Test Debugger.load handles uncaught exceptions in the debugged program.
class TestBreakpoints < Test::Unit::TestCase

  def test_basic
    Dir.chdir(SRC_DIR) do
      assert_equal(true,
                   run_debugger('raise', '--script raise.cmd raise.rb'))
    end
  end
end
