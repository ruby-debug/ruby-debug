#!/usr/bin/env ruby
require File.expand_path("../../../helper", __FILE__)

class TestCatchCommand < Test::Unit::TestCase
  class MockState
    attr_accessor :message 
    def context; end
    def confirm(msg); true end
    def print(*args)
      @message = *args
    end
  end
  
  # regression test for bug #20156
  def test_catch_does_not_blow_up
    state = MockState.new
    catch_cmd = Debugger::CatchCommand.new(state)
    assert(catch_cmd.match('catch off'))
    catch(:debug_error) do
      catch_cmd.execute
    end
    assert_equal(nil, state.message)
  end
end
