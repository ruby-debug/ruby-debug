#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

class TestTrace < Test::Unit::TestCase
  include TestHelper

  def test_trace_option
    filter = Proc.new{|got_lines, correct_lines|
      got_lines.collect!{|l| l =~ /gcd\.rb:/? l : nil}.compact!
      got_lines.each do |s|
        s.gsub!(/:.*gcd.rb:/, ':gcd.rb:')
      end
    }

    assert(run_debugger("trace", "-nx --trace ./example/gcd.rb 3 5",
                        :filter => filter))
  end

  def test_linetrace_command
    filter = Proc.new{|got_lines, correct_lines|
      got_lines.reject! {|l| l =~ /:(rdbg|linecache|interface)\.rb:/ }
    }

    testname = 'linetrace'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5",
                        :filter => filter))
  end

  def test_linetrace_plus_command
    filter = Proc.new{|got_lines, correct_lines|
      got_lines.reject! {|l| l =~ /:(rdbg|linecache|interface)\.rb:/ }
    }

    testname = 'linetracep'
    script = File.join('data', testname + '.cmd')
    assert(run_debugger(testname, "--script #{script} -- ./example/gcd.rb 3 5",
                        :filter => filter))
  end
end
