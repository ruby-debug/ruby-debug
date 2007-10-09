$: << File.join(File.dirname(__FILE__), '..', 'lib')
require 'test/unit'
require 'ruby_debug_base.jar'

class DebuggerTest < Test::Unit::TestCase

  def test_that_module_is_found
    Debugger.start
    Debugger.stop
  end 

end