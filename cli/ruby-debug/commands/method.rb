module Debugger
  class MethodCommand < Command # :nodoc:
    def regexp
      /^\s*m(?:ethod)?\s+(?:(i(:?nstance)?)\s)?/
    end

    def execute
      if @match[1]
        obj = debug_eval(@match.post_match)

        len = 0
        for v in obj.methods.sort
          len += v.size + 1
          if len > 70
            len = v.size + 1
            print "\n"
          end
          print "%s ", v
        end
        print "\n"
      else
        obj = debug_eval(@match.post_match)
        unless obj.kind_of? Module
          print "Should be Class/Module: %s\n", @match.post_match
        else
          len = 0
          for v in obj.instance_methods(false).sort
            len += v.size + 1
            if len > 70
              len = v.size + 1
              print "\n"
            end
            print "%s ", v
          end
          print "\n"
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