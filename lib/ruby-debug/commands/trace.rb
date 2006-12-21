module Debugger
  class TraceCommand < Command # :nodoc:
    def regexp
      /^\s*tr(?:ace)?(?:\s+(on|off))?(?:\s+(all))?$/
    end

    def execute
      if @match[2]
        Debugger.tracing = @match[1] == 'on'
      elsif @match[1]
        Debugger.current_context.tracing = @match[1] == 'on'
      end
      if Debugger.tracing || Debugger.current_context.tracing
        print_msg "Trace on."
      else
        print_msg "Trace off."
      end
    end

    class << self
      def help_command
        'trace'
      end

      def help(cmd)
        %{
          tr[ace] (on|off)\tset trace mode of current thread
          tr[ace] (on|off) all\tset trace mode of all threads
        }
      end
    end
  end
end