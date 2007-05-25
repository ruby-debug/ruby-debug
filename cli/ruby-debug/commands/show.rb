module Debugger
  class ShowCommand < Command # :nodoc:
    include ParseFunctions
    include ShowFunctions
    
    SubcmdStruct=Struct.new(:name, :min, :short_help)
    Subcommands = 
      [
       ['autoeval', 4, "Show if unrecognized command are evaluated"],
       ['autolist', 4, "Show if 'list' commands is run on breakpoints"],
       ['autoirb', 4, "Show if IRB is invoked on debugger stops"],
       ['autoreload', 4, "Show if source code is reloaded when changed"],
       ['forcestep', 1, "Show if sure 'next/step' forces move to a new line"],
       ['framefullpath', 1, "Show if full file names are displayed in frames"],
       ['trace', 1, 
        "Show if a stack trace is displayed when 'eval' raises exception"],
       ['width', 1, 
        "Show the number of characters the debugger thinks are in a line"],
      ].map do |name, min, short_help| 
      SubcmdStruct.new(name, min, short_help)
    end
    
    self.control = true

    def regexp
      /^show \s+ (.+) \s*/xi
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
        print "Unknown set command #{subcmd}\n"
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
