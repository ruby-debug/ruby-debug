module Debugger
  class MethodCommand < Command # :nodoc:
    def regexp
      /^\s*m(?:ethod)?\s+(?:(i(:?nstance)?)\s)?/
    end

    def execute
      if @match[1]
        obj = debug_eval(@match.post_match)
        print_methods(obj.methods)
      else
        obj = debug_eval(@match.post_match)
        unless obj.kind_of? Module
          print_msg "Should be Class/Module: %s", @match.post_match
        else
          print_methods(obj.instance_methods(false))
        end
      end
    end

    class << self
      def help_command
        'method'
      end

      def help(cmd)
        %{
          m[ethod] i[nstance] <obj>\tshow methods of object
          m[ethod] <class|module>\t\tshow instance methods of class or module
        }
      end
    end
  end
end