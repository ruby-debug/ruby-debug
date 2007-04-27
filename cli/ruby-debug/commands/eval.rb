module Debugger
  module EvalFunctions # :nodoc:
    def run_with_binding
      binding = @state.context ? get_binding : TOPLEVEL_BINDING
      $__dbg_interface = @state.interface
      eval(<<-EOC, binding)
        def dbg_print(*args)
          $__dbg_interface.print(*args)
        end
        def dbg_puts(*args)
          $__dbg_interface.print(*args)
          $__dbg_interface.print("\n")
        end
      EOC
      yield binding
    ensure
      $__dbg_interface = nil
    end
  end
  
  class EvalCommand < Command # :nodoc:
    self.control = true
    
    include EvalFunctions

    register_setting_get(:autoeval) do
      EvalCommand.unknown
    end
    register_setting_set(:autoeval) do |value|
      EvalCommand.unknown = value
    end

    def match(input)
      @input = input
      super
    end
    
    def regexp
      /^\s*(p|e(?:val)?)\s+/
    end

    def execute
      expr = @match ? @match.post_match : @input
      run_with_binding do |b|
        print "%s\n", debug_eval(expr, b).inspect
      end
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
            * NOTE - to turn on autoeval, use 'set autoeval'
          }
        end
      end
    end
  end

  class PPCommand < Command # :nodoc:
    self.control = true
    
    include EvalFunctions
    
    def regexp
      /^\s*pp\s+/
    end

    def execute
      out = StringIO.new
      run_with_binding do |b|
        PP.pp(debug_eval(@match.post_match, b), out)
      end
      print out.string
    rescue 
      out.puts $!.message
    end

    class << self
      def help_command
        'pp'
      end

      def help(cmd)
        %{
          pp expression\tevaluate expression and pretty-print its value
        }
      end
    end
  end
end
