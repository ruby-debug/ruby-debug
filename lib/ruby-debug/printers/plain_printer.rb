module Debugger
  class PlainPrinter # :nodoc:
    attr_accessor :interface
    
    def initialize(interface, is_debug=false)
      # plain_printer does not support debug yet
      @interface = interface
    end
    
    def print_msg(*args)
      print(*args)
      print "\n"
    end
    
    alias print_error print_msg
    
    def print_debug(*args)
    end
    
    def print_breakpoint_added(b)
      print_msg "Set breakpoint %d at %s:%s", b.id, b.source, b.pos
    end
    
    def print_breakpoint_deleted(b)
      print_msg "Deleted breakpoint %d", b.id
    end
    
    def print_current_frame(context, frame_pos)
      print_frame(context, frame_pos, frame_pos)
      if ENV['EMACS']
        file, line = context.frame_file(frame_pos), context.frame_line(frame_pos)
        print "\032\032%s:%d\n", file, line
      end
    end
    
    def print_frames(context, cur_idx)
      (0...context.stack_size).each do |idx|
        print_frame(context, idx, cur_idx)
      end
    end
    
    def print_frame(context, idx, cur_idx)
      file, line, id = context.frame_file(idx), context.frame_line(idx), context.frame_id(idx)
      if idx == cur_idx
        print "--> "
      else
        print "    "
      end
      print "#%d %s:%s%s\n", idx, file, line, (id ? ":in `#{id.id2name}'" : "")
    end
    
    def print_contexts(contexts)
      contexts.each do |c|
        print_context(c)
      end
    end
    
    def print_context(c)
      c_flag = c.thread == Thread.current ? '+' : ' '
      c_flag = '$' if c.suspended?
      d_flag = c.ignored? ? '!' : ' '
      print "%s%s", c_flag, d_flag
      print "%d ", c.thnum
      print "%s\t", c.thread.inspect
      print "%s:%d", c.frame_file(0), c.frame_line(0) if c.stack_size > 0
      print "\n"
    end
    
    def print_variables(vars, kind)
      vars.sort.each do |v|
        print_variable(v, yield(v).inspect, kind)
      end
    end
    
    def print_array(array)
      index = 0 
      array.each { |e|
        print_variable('[' + index.to_s + ']', e) 
        index += 1 
      }
    end
    
    def print_hash(hash)
      hash.keys.each { | k |
        if k.class.name == "String"
          name = '\'' + k + '\''
        else
          name = k.to_s
        end
        print_variable(name, hash[k]) 
      }
    end
    
    def print_variable(name, value, kind=nil)
      print "  %s => %s\n", name, value
    end
    
    def print_breakpoints(breakpoints)
      unless breakpoints.empty?
        print "Breakpoints:\n"
        breakpoints.sort_by{|b| b.id }.each do |b|
          if b.expr.nil?
            print "  %d %s:%s\n", b.id, b.source, b.pos
          else
            print "  %d %s:%s if %s\n", b.id, b.source, b.pos, b.expr
          end
        end
        print "\n"
      else
        print "No breakpoints\n"
      end
    end
    
    def print_expressions(exps)
      exps.each_with_index do |(exp, value), idx|
        print_expression(exp, value, idx+1)
      end
    end
    
    def print_expression(exp, value, idx)
      print "%d: %s = %s\n", idx, exp, value
    end
    
    def print_eval(exp, value)
      print "%s\n", value
    end
    
    def print_pp(exp, value)
      print value
    end
    
    def print_list(b, e, file, line)
      print "[%d, %d] in %s\n", b, e, file
      if lines = Debugger.source_for(file)
        n = 0
        b.upto(e) do |n|
          if n > 0 && lines[n-1]
            if n == line
              print "=> %d  %s\n", n, lines[n-1].chomp
            else
              print "   %d  %s\n", n, lines[n-1].chomp
            end
          end
        end
      else
        print "No sourcefile available for %s\n", file
      end
    end
    
    def print_methods(methods)
      len = 0
      for v in methods.sort
        len += v.size + 1
        if len > 70
          len = v.size + 1
          print "\n"
        end
        print "%s ", v
      end
      print "\n"
    end
    
    # Events
    
    def print_breakpoint(n, breakpoint)
      print("Breakpoint %d at %s:%s\n", n, breakpoint.source, breakpoint.pos)
    end
    
    def print_catchpoint(excpt)
      context = Debugger.current_context
      print "Catchpoint at %s:%d: `%s' (%s)\n", context.frame_file(0), context.frame_line(0), excpt, excpt.class
      fs = context.stack_size
      tb = caller(0)[-fs..-1]
      if tb
        for i in tb
          print "\tfrom %s\n", i
        end
      end
    end
    
    def print_trace(context, file, line)
      print "Tracing(%d):%s:%s %s", context.thnum, file, line, Debugger.line_at(file, line)
    end
    
    def print_at_line(file, line)
      print "\032\032" if ENV['EMACS']
      print "%s:%d: %s", file, line, Debugger.line_at(file, line)
    end
    
    def print_exception(excpt, binding)
      at = eval("caller(1)", binding)
      print "%s:%s\n", at.shift, excpt.to_s.sub(/\(eval\):1:(in `.*?':)?/, '')
      for i in at
        print "\tfrom %s\n", i
      end
    end
    
    private
    
    def print(*params)
      @interface.print(*params)
    end
    
    def print_element(name)
      yield
    end
    
  end
end