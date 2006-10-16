module Debugger
  class ListCommand < Command # :nodoc:
    def regexp
      /^\s*l(?:ist)?(?:\s*(.+))?$/
    end

    def execute
      if !@match || !@match[1]
        b = @state.previous_line ? @state.previous_line + 10 : @state.line - 5
        e = b + 9
      elsif @match[1] == '-'
        b = @state.previous_line ? @state.previous_line - 10 : @state.line - 5
        e = b + 9
      elsif @match[1] == '='
        @state.previous_line = nil
        b = @state.line - 5
        e = b + 9
      elsif @match[1] == 'on'
        self.class.always_run = true
        print_msg "Listing is on."
        return
      elsif @match[1] == 'off'
        self.class.always_run = false
        print_msg "Listing is off."
        return
      else
        b, e = @match[1].split(/[-,]/)
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
end