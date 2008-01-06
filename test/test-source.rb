#!/usr/bin/env ruby
require "test/unit"

# require "rubygems"
# require "ruby-debug"; Debugger.start

SRC_DIR = File.expand_path(File.dirname(__FILE__)) + "/" unless 
  defined?(SRC_DIR)

require File.join(SRC_DIR, "helper.rb")

include TestHelper

# Test annotate handling.
class TestAnnotate < Test::Unit::TestCase
  require 'stringio'

  def test_basic
    Dir.chdir(SRC_DIR) do 
      assert_equal(true, 
                   run_debugger("source", 
                                "--script source.cmd -- gcd.rb 3 5"))
    end
  end
end
