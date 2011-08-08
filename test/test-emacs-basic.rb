#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test the --emacs-basic option.
class TestEmacsBasic < Test::Unit::TestCase
  include TestHelper

  def test_basic
    testname='emacs_basic'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--emacs-basic --script #{script} -- ./example/gcd.rb 3 5"))
  end
end
