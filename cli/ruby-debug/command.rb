module Debugger

  module Columnize
    # Display a list of strings as a compact set of columns.
    #
    #  Each column is only as wide as necessary.
    #  Columns are separated by two spaces (one was not legible enough).
    #  Adapted from the routine of the same name in cmd.py
    def columnize(list, displaywidth=80)
      if not list.is_a?(Array)
        return "Expecting an Array, got #{list.class}\n"
      end
      if list.size == 0
        return  "<empty>\n"
      end
      nonstrings = []
      for str in list do
        nonstrings << str unless str.is_a?(String)
      end
      if nonstrings.size > 0
        return "Nonstrings: %s\n" % nonstrings.map {|non| non.to_s}.join(',')
      end
      if 1 == list.size
        return "#{list[0]}\n"
      end
      # Try every row count from 1 upwards
      nrows = ncols = 0
      colwidths = []
      1.upto(list.size) do 
        colwidths = []
        nrows += 1
        ncols = (list.size + nrows-1) / nrows
        totwidth = -2
        # Debugger.debugger if nrows > 1
        0.upto(ncols-1) do |col|
          colwidth = 0
          0.upto(nrows-1) do |row|
            i = row + nrows*col
            if i >= list.size
              break
            end
            colwidth = [colwidth, list[i].size].max
          end
          colwidths << colwidth
          totwidth += colwidth + 2
          if totwidth > displaywidth
            break
          end
        end
        if totwidth <= displaywidth
          break
        end
      end
      s = ''
      0.upto(nrows-1) do |row| 
        texts = []
        0.upto(ncols-1) do |col|
          i = row + nrows*col
          if i >= list.size
            x = ""
          else
            x = list[i]
          end
          texts << x
        end
        while texts and texts[-1] == ''
          texts = texts[0..-2]
        end
        0.upto(texts.size-1) do |col|
          texts[col] = texts[col].ljust(colwidths[col])
        end
        s += "%s\n" % texts.join("  ")
      end
      return s
    end
  end
  # Mix-in module to showing settings
  module ShowFunctions # :nodoc:
    def show_setting(setting_name)
      case setting_name
      when /^args$/
        args = Command.settings[:argv][1..-1].join(' ')
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

  # Mix-in module to assist in command parsing.
  module FrameFunctions # :nodoc:
    def adjust_frame(frame_pos, absolute)
      if absolute
        if frame_pos < 0
          abs_frame_pos = @state.context.stack_size + frame_pos
        else
          abs_frame_pos = frame_pos
        end
      else
        abs_frame_pos = @state.frame_pos + frame_pos
      end

      if abs_frame_pos >= @state.context.stack_size then
        print "Adjusting would put us beyond the oldest (initial) frame."
        return
      elsif abs_frame_pos < 0 then
        print "Adjusting would put us beyond the newest (innermost) frame."
        return
      end
      if @state.frame_pos != abs_frame_pos then
        @state.previous_line = nil
        @state.frame_pos = abs_frame_pos
      end
      
      @state.file = @state.context.frame_file(@state.frame_pos)
      @state.line = @state.context.frame_line(@state.frame_pos)
      
      print_frame(@state.frame_pos, true)
    end
    
    def get_frame_call(prefix, pos)
      id = @state.context.frame_method(pos)
      call_str = ""
      if id
        args = @state.context.frame_args(pos)
        call_str << "#{klass}." if Command.settings[:frame_class_names] && klass
        call_str << id.id2name
        if args.size > 0
          call_str << "("
          binding = @state.context.frame_binding(pos)
          args.each do |name|
            sep = ":"
            klass = eval("#{name}.class", binding)
            if klass.inspect.size > 20+3
              klass = klass.inspect[0..20]+"..." 
            end
            call_str += sprintf "%s#{sep}%s, " % [name, klass]
            if call_str.size > self.class.settings[:width] - prefix.size
              # Strip off trailing ', ' if any but add stuff for later trunc
              call_str[-2..-1] = ",...XX"
              break
            end
          end
          call_str[-2..-1] = ")" # Strip off trailing ', ' if any 
        end
      end
      return call_str
    end

    def print_frame(pos, adjust = false)
      file = @state.context.frame_file(pos)
      line = @state.context.frame_line(pos)
      klass = @state.context.frame_class(pos)

      unless Command.settings[:frame_full_path]
        path_components = file.split(/[\\\/]/)
        if path_components.size > 3
          path_components[0...-3] = '...'
          file = path_components.join(File::ALT_SEPARATOR || File::SEPARATOR)
        end
      end

      s = "#%d  " % pos
      call_str = get_frame_call(s, pos)
      file_line = " at line %s:%d\n" % [file, line]
      print s
      if call_str.size > 0
        print call_str
        if call_str.size + s.size + file_line.size > self.class.settings[:width]
          print "\n       "
        end
      end
      print " at line %s:%d\n", CommandProcessor.canonic_file(file), line
      print "\032\032%s:%d\n" % [CommandProcessor.canonic_file(file), 
                                 line] if ENV['EMACS'] && adjust
    end
  end

  module ParseFunctions
    # Parse 'str' of command 'cmd' as an integer between
    # min and max. If either min or max is nil, that
    # value has no bound.
    def get_int(str, cmd, min=nil, max=nil, default=1)
      return default unless str
      begin
        int = Integer(str)
        if min and int < min
          print "%s argument '%s' needs to at least %s.\n" % [cmd, str, min]
          return nil
        elsif max and int > max
          print "%s argument '%s' needs to at most %s.\n" % [cmd, str, max]
          return nil
        end
        return int
      rescue
        print "%s argument '%s' needs to be a number.\n" % [cmd, str]
        return nil
      end
    end

    # Return true if arg is 'on' or 1 and false arg is 'off' or 0.
    # Any other value raises RuntimeError.
    def get_onoff(arg, default=nil, print_error=true)
      if arg.nil? or arg == ''
        if default.nil?
          if print_error
            print "Expecting 'on', 1, 'off', or 0. Got nothing.\n"
            raise RuntimeError
          end
          return default
        end
      end
      case arg.downcase
      when '1'
      when 'on'
        return true
      when '0'
      when 'off'
        return false
      else
        if print_error
          print "Expecting 'on', 1, 'off', or 0. Got: %s.\n" % arg.to_s
          raise RuntimeError
        end
      end
    end

    # Return 'on' or 'off' for supplied parameter. The parmeter should
    # be true, false or nil.
    def show_onoff(bool)
      if not [TrueClass, FalseClass, NilClass].member?(bool.class)
        return "??"
      end
      return bool ? 'on' : 'off'
    end
  end
  
  module ThreadFunctions # :nodoc:
    include ParseFunctions
    def display_context(c)
      c_flag = c.thread == Thread.current ? '+' : ' '
      c_flag = '$' if c.suspended?
      d_flag = c.ignored? ? '!' : ' '
      print "%s%s", c_flag, d_flag
      print "%d ", c.thnum
      print "%s\t", c.thread.inspect
      if c.stack_size > 0
        print "%s:%d", c.frame_file(0), c.frame_line(0)
      end
      print "\n"
    end
    
    def parse_thread_num(subcmd, arg)
      if '' == arg
        print "'thread %s' needs a thread number\n" % subcmd
      else
        thread_num = get_int(arg, "thread #{subcmd}", 1)
        return nil unless thread_num
        c = get_context(thread_num)
        case 
        when nil == c
          print "No such thread.\n"
        when @state.context == c
          print "It's the current thread.\n"
        when c.ignored?
          print "Can't #{subcmd} to the debugger thread.\n"
        else # Everything is okay
          return c
        end
      end
      return nil
    end
  end

  module VarFunctions # :nodoc:
    def var_list(ary, b = get_binding)
      ary.sort!
      for v in ary
        print "  %s => %p\n", v, debug_eval(v, b)
      end
    end
  end

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

      def settings_map
        @@settings_map ||= {}
      end
      private :settings_map
      
      def settings
        unless @settings
          @settings = Object.new
          map = settings_map
          class << @settings; self end.send(:define_method, :[]) do |name|
            raise "No such setting #{name}" unless map.has_key?(name)
            map[name][:getter].call
          end
          class << @settings; self end.send(:define_method, :[]=) do |name, value|
            raise "No such setting #{name}" unless map.has_key?(name)
            map[name][:setter].call(value)
          end
        end
        @settings
      end

      def register_setting_var(name, default)
        var_name = "@@#{name}"
        class_variable_set(var_name, default)
        register_setting_get(name) { class_variable_get(var_name) }
        register_setting_set(name) { |value| class_variable_set(var_name, value) }
      end

      def register_setting_get(name, &block)
        settings_map[name] ||= {}
        settings_map[name][:getter] = block
      end

      def register_setting_set(name, &block)
        settings_map[name] ||= {}
        settings_map[name][:setter] = block
      end
    end

    register_setting_var(:basename, false)  # use basename in showing files? 
    register_setting_var(:force_stepping, false)
    register_setting_var(:frame_class_names, false)
    register_setting_var(:frame_full_path, true)
    register_setting_var(:listsize, 10)    # number of lines in list command
    register_setting_var(:stack_trace_on_error, false)
    register_setting_var(:width, 80)       # width of line output

    if not defined? Debugger::ARGV
      Debugger::ARGV = ARGV.clone
    end
    register_setting_var(:argv, Debugger::ARGV)
    
    def initialize(state)
      @state = state
    end

    def match(input)
      @match = regexp.match(input)
    end

    protected

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
        if Command.settings[:stack_trace_on_error]
          at = eval("caller(1)", b)
          print "%s:%s\n", at.shift, e.to_s.sub(/\(eval\):1:(in `.*?':)?/, '')
          for i in at
            print "\tfrom %s\n", i
          end
        else
          print "#{e.class} Exception: #{e.message}\n"
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

    def get_binding
      @state.context.frame_binding(@state.frame_pos)
    end

    def line_at(file, line)
      Debugger.line_at(file, line)
    end

    def get_context(thnum)
      Debugger.contexts.find{|c| c.thnum == thnum}
    end  
  end
  
  Command.load_commands

  # Returns setting object.
  # Use Debugger.settings[] and Debugger.settings[]= methods to query and set
  # debugger settings. These settings are available:
  # 
  # - :autolist - automatically calls 'list' command on breakpoint
  # - :autoeval - evaluates input in the current binding if it's not recognized as a debugger command
  # - :autoirb - automatically calls 'irb' command on breakpoint
  # - :stack_trace_on_error - shows full stack trace if eval command results with an exception
  # - :frame_full_path - displays full paths when showing frame stack
  # - :frame_class_names - displays method's class name when showing frame stack
  # - :reload_source_on_change - makes 'list' command to always display up-to-date source code
  # - :force_stepping - stepping command asways move to the new line
  # 
  def self.settings
    Command.settings
  end
end
