module Debugger
  class QuitCommand < Command # :nodoc:
    self.control = true

    def regexp
      /^\s*(?:q(?:uit)?|exit)\s*$/
    end

    def execute
      if confirm("Really quit? (y/n) ")
        Debugger.save_history if Debugger.respond_to? :save_history
        exit! # exit -> exit!: No graceful way to stop threads...
      end
    end

    class << self
      def help_command
        %w[quit exit]
      end

      def help(cmd)
        %{
          q[uit]\texit from debugger, 
          exit\talias to quit
        }
      end
    end
  end
  
  class RestartCommand < Command # :nodoc:
    self.control = true

    def regexp
      / ^\s*
      (?:restart|R)
      (\s+ \S+ .*)?
      $
      /ix
    end
    
    def execute
      if not defined? Debugger::RDEBUG_SCRIPT
        # FIXME? Should ask for confirmation? 
        print "Debugger was not called from the outset...\n"
        prog_script = $0
      else
        prog_script = Debugger::RDEBUG_SCRIPT
      end
      if not File.exists?(prog_script)
        print "Ruby program #{prog_script} doesn't exist\n"
        return
      end
      if not File.executable?(prog_script)
        print "Ruby program #{prog_script} doesn't seem to be executable...\n"
        print "We'll add a call to Ruby.\n"
        prog_script = "ruby -I#{$:.join(' -I')} #{prog_script}"
      end
      if @match[1]
        args = prog_script + " " + @match[1]
      else
        if not defined? Command.settings[:argv]
          print "Arguments have not been set. Use 'set args' to set them.\n"
          return
        else
          argv = Command.settings[:argv]
        end
        args = argv.join(" ")
      end

      # An execv would be preferable to the "exec" below.
      cmd = prog_script + " " + args
      print "Re exec'ing:\n\t#{cmd}\n"
      exec cmd
    rescue Errno::EOPNOTSUPP
      print "Restart command is not available at this time.\n"
    end

    class << self
      def help_command
        'restart'
      end

      def help(cmd)
        %{
          restart|R [args] 
          Restart the program. This is is a re-exec - all debugger state
          is lost. If command arguments are passed those are used.
        }
      end
    end
  end

  class RunCommand < Command # :nodoc:
    self.control = true

    def regexp
      / ^\s*
      (?:run)
      (?:\s+ (\S?.*\S))? \s*
      $
      /ix
    end
    
    def execute
      if not defined? Debugger::RDEBUG_SCRIPT
        print "This kind of restart works only if rdebug is called initially\n"
        return
      end
      if @match[1]
        Command.settings[:argv] = [Debugger::RDEBUG_SCRIPT] + @match[1].split(/[ \t]+/)
      end
      Debugger.abort_on_exception_save = Thread.abort_on_exception 
      Thread.abort_on_exception = true
      raise DebuggerRestart
    end

    class << self
      def help_command
        'run'
      end

      def help(cmd)
        %{
          run [args] 
         A "warm" restart of the program. In contrast to "restart",
this is is not re-exec - debugger settings are preserved. 
If command arguments are passed those are used.
        }
      end
    end
  end

  class InterruptCommand < Command # :nodoc:
    self.event = false
    self.control = true
    self.need_context = true
    
    def regexp
      /^\s*i(?:nterrupt)?\s*$/
    end
    
    def execute
      unless Debugger.interrupt_last
        context = Debugger.thread_context(Thread.main)
        context.interrupt
      end
    end
    
    class << self
      def help_command
        'interrupt'
      end
      
      def help(cmd)
        %{
          i[nterrupt]\tinterrupt the program
        }
      end
    end
  end
end
