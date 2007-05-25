module Debugger
  class SetCommand < Command # :nodoc:
    include ParseFunctions
    
    SubcmdStruct=Struct.new(:name, :min, :short_help)
    Subcommands = 
      [
       ['autoeval', 4, "Evaluate every unrecognized command"],
       ['autolist', 4, "Execute 'list' command on every breakpoint"],
       ['autoirb', 4, "Invoke IRB on every stop"],
       ['autoreload', 4, "Reload source code when changed"],
       ['forcestep', 1, "Make sure 'next/step' commands always move to a new line"],
       ['framefullpath', 1, "Display full file names in frames"],
       ['trace', 1, "Display stack trace when 'eval' raises exception"],
       ['width', 1, "Number of characters the debugger thinks are in a line"],
      ].map do |name, min, short_help| 
      SubcmdStruct.new(name, min, short_help)
    end
    
    self.control = true

    def regexp
      /^set \s+ (.+) \s*/xi
    end

    def execute
      if not @match[1]
        print "\"set\" must be followed by the name of an set command:\n"
        print "List of info subcommands:\n\n"
        for subcmd in Subcommands do
          print "set #{subcmd.name} -- #{subcmd.short_help}\n"
        end
      else
        subcmd, arg = @match[1].split(/[ \t]+/)
        subcmd.downcase!
        if subcmd =~ /^no/i
          set_on = false
          subcmd = subcmd[2..-1]
        else
          set_on = true
        end
        for try_subcmd in Subcommands do
          if (subcmd.size >= try_subcmd.min) and
              (try_subcmd.name[0..subcmd.size-1] == subcmd)
            case try_subcmd.name
            when /^autolist$/
              Command.settings[:autolist] = set_on
              print "autolist is #{show_onoff(set_on)}.\n"
            when /autoeval$/
              Command.settings[:autoeval] = set_on
              print "autoeval is #{show_onoff(set_on)}.\n"
            when /trace$/
              Command.settings[:stack_trace_on_error] = set_on
              print "Displaying stack trace is #{show_onoff(set_on)}.\n"
            when /framefullpath$/
              Command.settings[:frame_full_path] = set_on
              print "Displaying frame's full file names is #{show_onoff(set_on)}.\n"
            when /frameclassname$/
              Command.settings[:frame_class_names] = set_on
              print "Displaying frame's original class name is #{show_onoff(set_on)}.\n"
            when /autoreload$/
              Command.settings[:reload_source_on_change] = set_on
              print "autoreload is #{show_onoff(set_on)}.\n"
            when /autoirb$/
              Command.settings[:autoirb] = set_on
              print "autoirb is #{show_onoff(set_on)}.\n"
            when /forcestep$/
              self.class.settings[:force_stepping] = set_on
              print "force-stepping is #{show_onoff(set_on)}.\n"
            when /^width$/
              width = get_int(arg, "Set width")
              if width
                self.class.settings[:width] = width
                ENV['COLUMNS'] = width.to_s
                print "width is #{width}.\n"
              end
            else
              print "Unknown setting #{@match[1]}.\n"
            end
            return
          end
        end
        print "Unknown set command #{subcmd}\n"
      end
    end

    class << self
      def help_command
        "set"
      end

      def help(cmd)
        s = "
Modifies parts of the ruby-debug environment.
You can see these environment settings with the \"show\" command.

-- 
List of set subcommands:
--  
"
        for subcmd in Subcommands do
          s += "set #{subcmd.name} -- #{subcmd.short_help}\n"
        end
        return s
      end
    end
  end
end
