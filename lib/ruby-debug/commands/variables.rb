module Debugger
  class VarConstantCommand < Command # :nodoc:
    def regexp
      /^\s*v(?:ar)?\s+c(?:onst(?:ant)?)?\s+/
    end

    def execute
      obj = debug_eval(@match.post_match)
      unless obj.kind_of? Module
        print_msg "Should be Class/Module: %s", @match.post_match
      else
        print_variables(obj.constants, obj.module_eval{binding()}, "constant")
      end
    end

    class << self
      def help_command
        'var'
      end

      def help(cmd)
        %{
          v[ar] c[onst] <object>\t\tshow constants of object
        }
      end
    end
  end

  class VarGlobalCommand < Command # :nodoc:
    def regexp
      /^\s*v(?:ar)?\s+g(?:lobal)?\s*$/
    end

    def execute
      print_variables(global_variables, @state.binding, 'global')
    end

    class << self
      def help_command
        'var'
      end

      def help(cmd)
        %{
          v[ar] g[lobal]\t\t\tshow global variables
        }
      end
    end
  end

  class VarInstanceCommand < Command # :nodoc:
    def regexp
      /^\s*v(?:ar)?\s+i(?:nstance)?\s+/
    end

    def execute
      obj = debug_eval(@match.post_match)
      print_variables(obj.instance_variables, obj.instance_eval{binding()}, 'instance')
    end

    class << self
      def help_command
        'var'
      end

      def help(cmd)
        %{
          v[ar] i[nstance] <object>\tshow instance variables of object
        }
      end
    end
  end

  class VarLocalCommand < Command # :nodoc:
    def regexp
      /^\s*v(?:ar)?\s+l(?:ocal)?\s*$/
    end

    def execute
      print_variables(eval("local_variables", @state.binding), @state.binding, 'local')
    end

    class << self
      def help_command
        'var'
      end

      def help(cmd)
        %{
          v[ar] l[ocal]\t\t\tshow local variables
        }
      end
    end
  end
end
