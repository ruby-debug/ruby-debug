#!/usr/bin/env ruby
require 'test/unit'
require 'fileutils'

# begin require 'rubygems' rescue LoadError end
# require 'ruby-debug'; Debugger.init

SRC_DIR = File.dirname(__FILE__) unless 
  defined?(SRC_DIR)

require File.join(SRC_DIR, 'helper')

include TestHelper

# Test annotate handling.
class TestAnnotate < Test::Unit::TestCase
  require 'stringio'

  def test_basic
    Dir.chdir(SRC_DIR) do 
      assert_equal(true, 
                   run_debugger('annotate',
                                '--script annotate.cmd -- gcd.rb 3 5'))
    end
  end
end
