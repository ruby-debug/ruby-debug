module Debugger
  class CatchCommand < Command # :nodoc:
    self.allow_in_control = true

    def regexp
      /^\s*cat(?:ch)?(?:\s+(.+))?$/
    end

    def execute
      if excn = @match[1]
        if excn == 'off'
          Debugger.catchpoint = nil
          print "Clear catchpoint.\n"
        else
          binding = @state.context ? get_binding : TOPLEVEL_BINDING
          unless debug_eval("#{excn}.is_a?(Class)", binding)
            print "Warning #{excn} is not known to be a Class\n"
          end
          Debugger.add_catchpoint(excn)
          print "Catch exception %s.\n", excn
        end
      else
        if Debugger.catchpoint
          print "Catchpoint %s.\n", Debugger.catchpoint
        else
          print "No catchpoint.\n"
        end
      end
    end

    class << self
      def help_command
        'catch'
      end

      def help(cmd)
        %{
          cat[ch]\t\t\tshow catchpoint
          cat[ch] <an Exception>\tset catchpoint to an exception
        }
      end
    end
  end
end
