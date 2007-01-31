module Debugger
  module ThreadFunctions # :nodoc:
    def display_context(c)
      c_flag = c.thread == Thread.current ? '+' : ' '
      d_flag = debugger_thread?(c) ? '!' : ' '
      print "%s%s", c_flag, d_flag
      print "%d ", c.thnum
      print "%s\t", c.thread.inspect
      if c.stack_size > 0
        print "%s:%d", c.frame_file(0), c.frame_line(0)
      end
      print "\n"
    end
    
    def debugger_thread?(c)
      [Debugger.thread, Debugger.control_thread].include?(c.thread)
    end
  end

  class ThreadListCommand < Command # :nodoc:
    self.control = true
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s+l(?:ist)?\s*$/
    end

    def execute
      threads = Debugger.contexts.sort_by{|c| c.thnum}.each do |c|
        display_context(c)
      end
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] l[ist]\t\t\tlist all threads
        }
      end
    end
  end

  class ThreadSwitchCommand < Command # :nodoc:
    self.control = true
    self.need_context = true
    
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s+(?:sw(?:itch)?\s+)?(\d+)\s*$/
    end

    def execute
      c = get_context(@match[1].to_i)
      case
      when c == @state.context
        print "It's the current thread.\n"
      when debugger_thread?(c)
        print "Can't switch to the debugger thread.\n"
      else
        display_context(c)
        c.stop_next = 1
        c.thread.run
        @state.proceed
      end
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] [sw[itch]] <nnn>\tswitch thread context to nnn
        }
      end
    end
  end

  class ThreadStopCommand < Command # :nodoc:
    self.control = true
    self.need_context = true
    
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s+stop\s+(\d+)\s*$/
    end

    def execute
      c = get_context(@match[1].to_i)
      case
      when c == @state.context
        print "It's the current thread.\n"
      when debugger_thread?(c)
        print "Can't stop the debugger thread.\n"
      when c.thread.stop?
        print "Already stopped.\n"
      else
        display_context(c)
        c.suspend
      end
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] stop <nnn>\t\tstop thread nnn
        }
      end
    end
  end

  class ThreadCurrentCommand < Command # :nodoc:
    self.need_context = true
    
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s+c(?:ur(?:rent)?)?\s*$/
    end

    def execute
      display_context(@state.context)
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] c[ur[rent]]\t\tshow current thread
        }
      end
    end
  end

  class ThreadResumeCommand < Command # :nodoc:
    self.control = true
    self.need_context = true
    
    include ThreadFunctions

    def regexp
      /^\s*th(?:read)?\s+resume\s+(\d+)\s*$/
    end

    def execute
      c = get_context(@match[1].to_i)
      case
      when c == @state.context
        print "It's the current thread.\n"
      when debugger_thread?(c)
        print "Can't resume the debugger thread.\n"
      when !c.thread.stop?
        print "Already running."
      else
        display_context(c)
        c.resume
      end
    end

    class << self
      def help_command
        'thread'
      end

      def help(cmd)
        %{
          th[read] resume <nnn>\t\tresume thread nnn
        }
      end
    end
  end
end
