module Debugger
  class ListCommand < Command # :nodoc:
    def regexp
      /^\s*l(?:ist)?(?:\s*([-=])|\s+(.+))?$/
    end

    def execute
      if !@match || !(@match[1] || @match[2])
        b = @state.previous_line ? @state.previous_line + 10 : @state.line - 5
        e = b + 9
      elsif @match[1] == '-'
        b = @state.previous_line ? @state.previous_line - 10 : @state.line - 5
        e = b + 9
      elsif @match[1] == '='
        @state.previous_line = nil
        b = @state.line - 5
        e = b + 9
      else
        b, e = @match[2].split(/[-,]/)
        if e
          b = b.to_i
          e = e.to_i
        else
          b = b.to_i - 5
          e = b + 9
        end
      end
      @state.previous_line = b
      print_list(b, e, @state.file, @state.line)
    end

    class << self
      def help_command
        'list'
      end

      def help(cmd)
        %{
          l[ist]\t\tlist forward
          l[ist] -\tlist backward
          l[ist] =\tlist current line
          l[ist] nn-mm\tlist given lines
          l[ist] on/off\tprint listing on every stop
        }
      end
    end
  end

  class ReloadCommand < Command # :nodoc
    self.control = true
    
    def regexp
      /^\s*r(?:eload)?$/
    end
    
    def execute
      Debugger.source_reload
      print_msg "Source code is reloaded. Automatic reloading is #{source_reloading}.\n"
    end
    
    private
    
    def source_reloading
      Debugger.reload_source_on_change ? 'on' : 'off'
    end
    
    class << self
      def help_command
        'reload'
      end

      def help(cmd)
        %{
          r[eload]\tforces source code reloading
        }
      end
    end
  end
end
