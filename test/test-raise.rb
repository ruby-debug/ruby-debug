#!/usr/bin/env ruby
require "test/unit"
require "fileutils"

# Note: The "right" file might not be 100% right! The intention,
# however, is to demonstrate that the debugger should not step into
# the debugger source code when the --no-quit option is used.
#
# This problem is probably best soled by adding a "rescue" caluse in
# Debugger.debug_load (i.e in the C source code).

# require "rubygems"
# require "ruby-debug"; Debugger.start

SRC_DIR = File.expand_path(File.dirname(__FILE__)) + "/" unless
  defined?(SRC_DIR)

require File.join(SRC_DIR, "helper.rb")

include TestHelper

# Test condition command
class TestBreakpoints < Test::Unit::TestCase
  require 'stringio'

  def test_basic
    Dir.chdir(SRC_DIR) do
      assert_equal(true,
                   run_debugger("raise",
                                "--no-quit --script raise.cmd -- raise.rb"))
    end
  end
end
