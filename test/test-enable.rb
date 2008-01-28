#!/usr/bin/env ruby
require 'test/unit'
require 'fileutils'

SRC_DIR = File.dirname(__FILE__) unless 
  defined?(SRC_DIR)
  
# begin require 'rubygems' rescue LoadError end
# require 'ruby-debug'; Debugger.init

# Test enable and disable commands
class TestEnable < Test::Unit::TestCase
  
  require File.join(SRC_DIR, 'helper')
  
  include TestHelper

  def test_basic
    Dir.chdir(SRC_DIR) do
      assert_equal(true, 
                   run_debugger('enable',
                                '--script enable.cmd -- gcd.rb 3 5'))
    end
  end
end
