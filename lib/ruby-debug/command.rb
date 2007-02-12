module Debugger
  class Command # :nodoc:
    class << self
      def commands
        @commands ||= []
      end
      
      DEF_OPTIONS = {
        :event => true, 
        :control => false, 
        :always_run => false,
        :unknown => false,
        :need_context => false,
      }
      
      def inherited(klass)
        DEF_OPTIONS.each do |o, v|
          klass.options[o] = v if klass.options[o].nil?
        end
        commands << klass
      end

      def load_commands
        dir = File.dirname(__FILE__)
        Dir[File.join(dir, 'commands', '*')].each do |file|
          require file if file =~ /\.rb$/
        end
      end
      
      def method_missing(meth, *args, &block)
        if meth.to_s =~ /^(.+?)=$/
          @options[$1.intern] = args.first
        else
          if @options.has_key?(meth)
            @options[meth]
          else
            super
          end
        end
      end
      
      def options
        @options ||= {}
      end
    end

    @@display_stack_trace = false
    
    def initialize(state, printer)
      @state, @printer = state, printer
    end

    def match(input)
      @match = regexp.match(input)
    end

    protected
    
    def method_missing(meth, *args, &block)
      if @printer.respond_to? meth
        @printer.send meth, *args, &block
      else
        super
      end
    end

    def print(*args)
      @state.print(*args)
    end

    def confirm(msg)
      @state.confirm(msg) == 'y'
    end

    def debug_eval(str, b = get_binding)
      begin
        val = eval(str, b)
      rescue StandardError, ScriptError => e
        if @@display_stack_trace
          @printer.print_exception(e, @state.binding) 
        else
          print_error "#{e.class} Exception: #{e.message}\n"
        end
        throw :debug_error
      end
    end

    def debug_silent_eval(str)
      begin
        eval(str, get_binding)
      rescue StandardError, ScriptError
        nil
      end
    end

    def hbinding(hash)
      code = hash.keys.map{|k| "#{k} = hash['#{k}']"}.join(';') + ';binding'
      if obj = @state.context.frame_self(@state.frame_pos)
        obj.instance_eval code
      else
        eval code
      end
    end
    private :hbinding
 
    def get_binding
      binding = @state.context.frame_binding(@state.frame_pos)
      binding || hbinding(@state.context.frame_locals(@state.frame_pos))
    end

    def line_at(file, line)
      Debugger.line_at(file, line)
    end

    def get_context(thnum)
      Debugger.contexts.find{|c| c.thnum == thnum}
    end
  end
  
  Command.load_commands
end
