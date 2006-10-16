module Debugger
  class AddDisplayCommand < Command # :nodoc:
    def regexp
      /^\s*disp(?:lay)?\s+(.+)$/
    end

    def execute
      exp = @match[1]
      @state.display.push [true, exp]
      print_expression(exp, debug_silent_eval(exp).to_s, @state.display.size)
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
    
    def regexp
      /^\s*disp(?:lay)?$/
    end

    def execute
      exps = @state.display.select{|status, exp| status }.map do |status, exp|
        [exp, debug_silent_eval(d[1]).to_s]
      end
      print_expressions(exps)
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
          print_msg "Display expression %d is not defined", pos
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