#!/usr/bin/env ruby
require File.expand_path("../helper", __FILE__)

# Test --no-stop and $0 setting.
class TestDollar0 < Test::Unit::TestCase
  include TestHelper

  def test_basic
    home_save = ENV['HOME']
    ENV['HOME'] = '.'
    filter = Proc.new{|got_lines, correct_lines|
      [got_lines, correct_lines].flatten.each do |s|
        s.gsub!(/.*dollar-0.rb$/, 'dollar-0.rb')
      end
    }

    assert(run_debugger('dollar-0', '-nx --no-stop ./example/dollar-0.rb',
                        :filter => filter,
                        :runner => '../bin/rdebug'))

    # Ruby's __FILE__ seems to prepend ./ when no directory was added.
    assert(run_debugger('dollar-0a', '-nx --no-stop ./example/dollar-0.rb',
                        :filter => filter,
                        :runner => '../bin/rdebug'))
    
    # Ruby's __FILE__ seems to prepend ./ when no directory was added.
    assert(run_debugger('dollar-0b', '-nx --no-stop ' + File.join(%w(example dollar-0.rb)),
                        :filter => filter,
                        :runner => '../bin/rdebug'))
  ensure
    ENV['HOME'] = home_save
  end
end
