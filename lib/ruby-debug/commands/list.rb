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
        print "Listing is on.\n"
        return
      elsif @match[1] == 'off'
        self.class.always_run = false
        print "Listing is off.\n"
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
      display_list(b, e, @state.file, @state.line)
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

    private

    def display_list(b, e, file, line)
      print "[%d, %d] in %s\n", b, e, file
      if lines = Debugger.source_for(file)
        n = 0
        b.upto(e) do |n|
          if n > 0 && lines[n-1]
            if n == line
              print "=> %d  %s\n", n, lines[n-1].chomp
            else
              print "   %d  %s\n", n, lines[n-1].chomp
            end
          end
        end
      else
        print "No sourcefile available for %s\n", file
      end
    end
  end
end