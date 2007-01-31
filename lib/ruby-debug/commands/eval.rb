module Debugger
  class EvalCommand < Command # :nodoc:
    def match(input)
      @input = input
      super
    end
    
    def regexp
      /^\s*(p|e(?:val)?)(?:\s+(on|off)$|\s+)/
    end

    def execute
      if @match && @match[1] != 'p' && %w[on off].include?(@match[2])
        self.class.unknown = @match[2] == 'on'
        print "Evaluation of unknown command is #{self.class.unknown ? 'on': 'off'}.\n"
        return
      end
      expr = @match ? @match.post_match : @input
      print "%s\n", debug_eval(expr).inspect
    end

    class << self
      def help_command
        %w|p eval|
      end

      def help(cmd)
        if cmd == 'p'
          %{
            p expression\tevaluate expression and print its value
          }
        else
          %{
            e[val] expression\tevaluate expression and print its value,
            \t\t\talias for p.
            e[val] on/off\t\twhen 'on', debugger will evaluate every unknown command.
          }
        end
      end
    end
  end

  class PPCommand < Command # :nodoc:
    def regexp
      /^\s*pp\s+/
    end

    def execute
      out = StringIO.new
      PP.pp(debug_eval(@match.post_match), out) rescue out.puts $!.message
      print out.string
    end

    class << self
      def help_command
        'pp'
      end

      def help(cmd)
        %{
          pp expression\tevaluate expression and print its value
        }
      end
    end
  end
end
