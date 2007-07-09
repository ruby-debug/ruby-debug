module Debugger
  # Mix-in module to showing settings
  module ShowFunctions # :nodoc:
    def show_setting(setting_name)
      case setting_name
      when /^args$/
        if Command.settings[:argv] and Command.settings[:argv].size > 0
          args = Command.settings[:argv][1..-1].join(' ')
        else
          args = ''
        end
        return "Argument list to give program being debugged when it is started is \"#{args}\"."
      when /^autolist$/
        on_off = Command.settings[:autolist]
        return "autolist is #{show_onoff(on_off)}."
      when /^autoeval$/
        on_off = Command.settings[:autoeval]
        return "autoeval is #{show_onoff(on_off)}."
      when /^autoreload$/
        on_off = Command.settings[:reload_source_on_change]
        return "autoreload is #{show_onoff(on_off)}."
      when /^autoirb$/
        on_off = Command.settings[:autoirb]
        return "autoirb is #{show_onoff(on_off)}."
      when /^basename$/
        on_off = Command.settings[:basename]
        return "basename is #{show_onoff(on_off)}."
      when /^forcestep$/
        on_off = self.class.settings[:force_stepping]
        return "force-stepping is #{show_onoff(on_off)}."
      when /^framefullpath$/
        on_off = Command.settings[:frame_full_path]
        return "Displaying frame's full file names is #{show_onoff(on_off)}."
      when /^frameclassname$/
        on_off = Command.settings[:frame_class_names]
        return "Displaying frame's original class name is #{show_onoff(on_off)}."
      when /^keep-frame-bindings$/
        on_off = Debugger.keep_frame_binding?
        return "keep-frame-bindings is #{show_onoff(on_off)}."
      when /^linetrace$/
        on_off = Debugger.tracing
        return "line tracing is #{show_onoff(on_off)}."
      when /^listsize$/
        listlines = Command.settings[:listsize]
        return "Number of source lines to list by default is #{listlines}."
      when /^port$/
        return "server port is #{Debugger::PORT}."
      when /^post-mortem$/
        on_off = Debugger.post_mortem
        return "post-mortem handling is #{show_onoff(on_off)}."
      when /^trace$/
        on_off = Command.settings[:stack_trace_on_error]
        return "Displaying stack trace is #{show_onoff(on_off)}."
      when /^version$/
        return "ruby-debug #{Debugger::VERSION}"
      when /^width$/
        return "width is #{self.class.settings[:width]}."
      else
        return "Unknown show subcommand #{setting_name}."
      end
    end
  end

  class ShowCommand < Command # :nodoc:
    
    SubcmdStruct=Struct.new(:name, :min, :short_help)
    Subcommands = 
      [
       ['args', 2, 
        "Show argument list to give program being debugged when it is started"],
       ['autoeval', 4, "Show if unrecognized command are evaluated"],
       ['autolist', 4, "Show if 'list' commands is run on breakpoints"],
       ['autoirb', 4, "Show if IRB is invoked on debugger stops"],
       ['autoreload', 4, "Show if source code is reloaded when changed"],
       ['basename', 1, "Show if basename used in reporting files"],
       ['forcestep', 1, "Show if sure 'next/step' forces move to a new line"],
       ['framefullpath', 1, "Show if full file names are displayed in frames"],
       ['keep-frame-bindings', 1, "Save frame binding on each call"],
       ['linetrace', 3, "Show line execution tracing"],
       ['listsize', 3, "Show number of source lines to list by default"],
       ['port', 1, "Show server port"],
       ['trace', 1, 
        "Show if a stack trace is displayed when 'eval' raises exception"],
       ['version', 1, 
        "Show what version of the debugger this is"],
       ['width', 1, 
        "Show the number of characters the debugger thinks are in a line"],
      ].map do |name, min, short_help| 
      SubcmdStruct.new(name, min, short_help)
    end
    
    self.control = true

    def regexp
      /^show (?: \s+ (.+) )?$/xi
    end

    def execute
      if not @match[1]
        print "\"show\" must be followed by the name of an show command:\n"
        print "List of show subcommands:\n\n"
        for subcmd in Subcommands do
          print "show #{subcmd.name} -- #{subcmd.short_help}\n"
        end
      else
        subcmd, arg = @match[1].split(/[ \t]+/)
        subcmd.downcase!
        for try_subcmd in Subcommands do
          if (subcmd.size >= try_subcmd.min) and
              (try_subcmd.name[0..subcmd.size-1] == subcmd)
            print "%s\n" % show_setting(try_subcmd.name)
            return
          end
        end
        print "Unknown show command #{subcmd}\n"
      end
    end

    class << self
      def help_command
        "show"
      end

      def help(cmd)
        s = "
          Generic command for showing things about the debugger.

          -- 
          List of show subcommands:
          --  
        "
        for subcmd in Subcommands do
          s += "show #{subcmd.name} -- #{subcmd.short_help}\n"
        end
        return s
      end
    end
  end
end
