module Debugger
  class SetCommand < Command # :nodoc:
    self.control = true

    def regexp
      /^set \s+ (.+) \s*/x
    end

    def execute
      case @match[1]
      when /^(no)?autolist$/
        Command.settings[:autolist] = $1.nil?
        print "autolist is #{$1.nil? ? 'on' : 'off'}.\n"
      when /^(no)?autoeval$/
        Command.settings[:autoeval] = $1.nil?
        print "autoeval is #{$1.nil? ? 'on' : 'off'}.\n"
      when /^(no)?trace$/
        Command.settings[:stack_trace_on_error] = $1.nil?
        print "Displaying stack trace is #{$1.nil? ? 'on' : 'off'}.\n"
      when /^(no)?framefullpath$/
        Command.settings[:frame_full_path] = $1.nil?
        print "Displaying frame's full file names is #{$1.nil? ? 'on' : 'off'}.\n"
      when /^(no)?frameclassname$/
        Command.settings[:frame_class_names] = $1.nil?
        print "Displaying frame's original class name is #{$1.nil? ? 'on' : 'off'}.\n"
      when /^(no)?autoreload$/
        Command.settings[:reload_source_on_change] = $1.nil?
        print "autoreload is #{$1.nil? ? 'on' : 'off'}.\n"
      when /^(no)?autoirb$/
        Command.settings[:autoirb] = $1.nil?
        print "autoirb is #{$1.nil? ? 'on' : 'off'}.\n"
      when /^(no)?forcestep$/
        self.class.settings[:force_stepping] = $1.nil?
        print "force-stepping is #{$1.nil? ? 'on' : 'off'}.\n"
      else
        print "Unknown setting.\n"
      end
    end

    class << self
      def help_command
        "set"
      end

      def help(cmd)
        %{
           set <setting>, where <setting>:
           autolist       - execute 'list' command on every breakpoint
           autoeval       - evaluate every unrecognized command
           autoreload     - enables automatic source code reloading
           autoirb        - debugger invokes IRB on every stop
           trace          - display stack trace when 'eval' raises exception
           framefullpath  - frame will display full file names
           frameclassname - frame will display class names
           forcestep      - make sure 'next/step' commands always move to a new line
           To disable setting, use 'no' prefix, like 'noautolist'
         }
      end
    end
  end
end
