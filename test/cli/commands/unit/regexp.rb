#!/usr/bin/env ruby
require File.expand_path("../../../../helper", __FILE__)

class TestCommandREs < Test::Unit::TestCase
  include Debugger
 
  def test_quit
    c = QuitCommand.new(nil)
    assert c.regexp.match('quit')
    assert c.regexp.match('q')
    assert c.regexp.match('quit!')
    assert c.regexp.match('q!')
    assert c.regexp.match('quit unconditionally')
    assert c.regexp.match('exit')
    assert c.regexp.match('exit!')
  end

  def test_up
    c = UpCommand.new(nil)
    assert c.regexp.match('up')
    assert c.regexp.match('up 2')
    assert c.regexp.match('up 2+5')
    assert c.regexp.match('u')
    assert c.regexp.match('u 2')
    assert_equal nil, c.regexp.match('ufoo')
  end

  def test_down
    c = DownCommand.new(nil)
    assert c.regexp.match('down')
    assert c.regexp.match('down 2')
    assert_equal(nil, c.regexp.match('d 2'))
    assert_equal(nil, c.regexp.match('d'))
    assert_equal(nil, c.regexp.match('dow'))
  end
end
