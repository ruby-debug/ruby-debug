#!/usr/bin/env ruby
require 'test/unit'

# require 'rubygems'
# require 'ruby-debug'; Debugger.start

# Test annotate handling.
class TestEmacsBasic < Test::Unit::TestCase

  @@SRC_DIR = File.dirname(__FILE__) unless 
  defined?(@@SRC_DIR)

  require File.join(@@SRC_DIR, 'helper.rb')
  
  include TestHelper
  
  require 'stringio'

  def test_basic
    Dir.chdir(@@SRC_DIR) do 
      assert_equal(true, 
                   run_debugger('emacs-basic', 
                                '--emacs-basic --script emacs-basic.cmd -- gcd.rb 3 5'))
    end
  end
end
