module Debugger
  module DisplayFunctions # :nodoc:
    def display_expression(exp)
      print "%s = %s\n", exp, debug_silent_eval(exp).to_s
    end
  end

  class AddDisplayCommand < Command # :nodoc:
    include DisplayFunctions

    def regexp
      /^\s*disp(?:lay)?\s+(.+)$/
    end

    def execute
      exp = @match[1]
      @state.display.push [true, exp]
      print "%d: ", @state.display.size
      display_expression(exp)
    end

    class << self
      def help_command
        'display'
      end

      def help(cmd)
        %{
          disp[lay] <expression>\tadd expression into display expression list
        }
      end
    end
  end

  class DisplayCommand < Command # :nodoc:
    self.always_run = true
    include DisplayFunctions
    
    def regexp
      /^\s*disp(?:lay)?$/
    end

    def execute
      n = 1
      for d in @state.display
        if d[0]
          print "%d: ", n
          display_expression(d[1])
        end
        n += 1
      end
    end

    class << self
      def help_command
        'display'
      end

      def help(cmd)
        %{
          disp[lay]\t\tdisplay expression list
        }
      end
    end
  end

  class DeleteDisplayCommand < Command # :nodoc:
    include DisplayFunctions

    def regexp
      /^\s*undisp(?:lay)?(?:\s+(\d+))?$/
    end

    def execute
      unless pos = @match[1]
        if confirm("Clear all expressions? (y/n) ")
          for d in @state.display
            d[0] = false
          end
        end
      else
        pos = pos.to_i
        if @state.display[pos-1]
          @state.display[pos-1][0] = false
        else
          print "Display expression %d is not defined\n", pos
        end
      end
    end

    class << self
      def help_command
        'undisplay'
      end

      def help(cmd)
        %{
          undisp[lay][ nnn]\tdelete one particular or all display expressions
        }
      end
    end
  end
end