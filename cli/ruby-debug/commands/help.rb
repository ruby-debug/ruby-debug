# Display a list of strings as a compact set of columns.
#
#  Each column is only as wide as necessary.
#  Columns are separated by two spaces (one was not legible enough).
#  Adapted from the routine of the same name in cmd.py

module Debugger
  class HelpCommand < Command # :nodoc:
    include Columnize
    self.control = true

    def regexp
      /^\s*h(?:elp)?(?:\s+(.+))?$/
    end

    def execute
      print "ruby-debug help v#{Debugger::VERSION}\n"
      cmds = @state.commands.select{ |cmd| [cmd.help_command].flatten.include?(@match[1]) }
      unless cmds.empty?
        help = cmds.map{ |cmd| cmd.help(@match[1]) }.join
        print help.split("\n").reject{|l| l =~ /^\s*$/ }.map{|l| l.gsub(/^ +/, '')}.join("\n")
      else
        print "Type 'help <command-name>' for help on a specific command\n\n"
        print "Available commands:\n"
        cmds = @state.commands.map{ |cmd| cmd.help_command }
        cmds = cmds.flatten.uniq.sort
        print columnize(cmds, self.class.settings[:width])
      end
      print "\n"
    end

    class << self
      def help_command
        'help'
      end

      def help(cmd)
        %{
          h[elp]\t\tprint this help
          h[elp] command\tprint help on command
        }
      end
    end
  end
end
