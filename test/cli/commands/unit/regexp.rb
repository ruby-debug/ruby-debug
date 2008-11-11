require 'test/unit'


class TestCommandREs < Test::Unit::TestCase
  base_dir=File.expand_path(File.join(File.dirname(__FILE__), 
                                      '..', '..', '..', '..',
                                      'cli', 'ruby-debug'))
  require File.join(base_dir, 'command')
  require File.join(base_dir, 'commands', 'frame')
  include Debugger
 
  def test_up
    c = UpCommand.new(nil)
    assert c.regexp.match('up')
    assert c.regexp.match('up 2')
    assert_equal nil, c.regexp.match('ufoo')
  end
end

