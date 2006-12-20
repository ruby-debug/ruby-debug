module Debugger
  module FrameFunctions # :nodoc:
    def format_frame(frame, pos)
      file, line, id = frame.file, frame.line, frame.id
      "#%d %s:%s%s\n" % [pos + 1, file, line, (id ? ":in `#{id.id2name}'" : "")]
    end
  end

  class WhereCommand < Command # :nodoc:
    include FrameFunctions

    def regexp
      /^\s*(?:w(?:here)?|f(?:rame)?)$/
    end

    def execute
      @state.frames.each_with_index do |frame, idx|
        if idx == @state.frame_pos
          print "--> "
        else
          print "    "
        end
        print format_frame(frame, idx)
      end
    end

    class << self
      def help_command
        %w|where frame|
      end

      def help(cmd)
        if cmd == 'where'
          %{
            w[here]\tdisplay frames
          }
        else
          %{
            f[rame]\t\talias for where
          }
        end
      end
    end
  end

  class UpCommand < Command # :nodoc:
    include FrameFunctions

    def regexp
      /^\s*(?:(up)(?:\s+(\d+))?|(f)(?:rame)?(?:\s+(\d+)))\s*$/
    end

    def execute
      if @match[1]
        cmd, arg = @match.captures
      else
        cmd, arg = @match.captures[2..-1]
      end
      @state.previous_line = nil
      if cmd == 'f'
        @state.frame_pos = arg.to_i - 1
      else
        @state.frame_pos += (arg ? arg.to_i : 1)
      end
      @state.frame_pos = 0 if @state.frame_pos < 0
      if @state.frame_pos >= @state.frames.size
        @state.frame_pos = @state.frames.size - 1
        print "At toplevel\n"
      end
      frame = @state.frames[@state.frame_pos]
      @state.binding, @state.file, @state.line = frame.binding, frame.file, frame.line
      print format_frame(frame, @state.frame_pos)
    end

    class << self
      def help_command
        %w|up frame|
      end

      def help(cmd)
        if cmd == 'up'
          %{
            up[ nn]\tmove to higher frame
          }
        else
          %{
            f[rame] n\tselect nth frame
          }
        end
      end
    end
  end

  class DownCommand < Command # :nodoc:
    include FrameFunctions

    def regexp
      /^\s*down(?:\s+(\d+))?$/
    end

    def execute
      @state.previous_line = nil
      @state.frame_pos -= @match[1] ? @match[1].to_i : 1
      if @state.frame_pos < 0
        @state.frame_pos = 0
        print "At stack bottom\n"
      end
      frame = @state.frames[@state.frame_pos]
      @state.binding, @state.file, @state.line = frame.binding, frame.file, frame.line
      print format_frame(frame, @state.frame_pos)
    end

    class << self
      def help_command
        'down'
      end

      def help(cmd)
        %{
          down[ nn]\tmove to lower frame
        }
      end
    end
  end
end