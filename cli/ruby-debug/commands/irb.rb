require 'irb'
module IRB # :nodoc:
  def self.start_session(binding)
    unless @__initialized
      args = ARGV
      ARGV.replace(ARGV.dup)
      IRB.setup(nil)
      ARGV.replace(args)
      @__initialized = true
    end
    
    workspace = WorkSpace.new(binding)

    irb = Irb.new(workspace)

    @CONF[:IRB_RC].call(irb.context) if @CONF[:IRB_RC]
    @CONF[:MAIN_CONTEXT] = irb.context

#     trap("SIGINT") do
#       irb.signal_handle
#     end

    catch(:IRB_EXIT) do
      irb.eval_input
    end
  end
end

module Debugger
  class IRBCommand < Command # :nodoc:
    def regexp
      /^irb$/
    end
    
    def execute
      unless @state.interface.kind_of?(LocalInterface)
        print "Command is available only in local mode.\n"
        throw :debug_error
      end
      IRB.start_session(get_binding)
    end
    
    class << self
      def help_command
        'irb'
      end

      def help(cmd)
        %{
          irb\tstarts an IRB session. (EXPERIMENTAL)
        }
      end
    end
  end
end

