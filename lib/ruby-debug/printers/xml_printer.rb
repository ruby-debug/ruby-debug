require 'cgi'

module Debugger
  class XmlPrinter # :nodoc:
    attr_accessor :interface
    
    def initialize(interface)
      @interface = interface
    end

    def print_msg(*args)
      msg, *args = args
      xml_message = CGI.escapeHTML(msg % args)
      print "<message>#{xml_message}</message>\n"
    end
    
    def print_error(*args)
      print_element("error") do
        msg, *args = args
        print CGI.escapeHTML(msg % args)
      end
    end
    
    def print_frames(frames, cur_idx)
      print_element("frames") do
        frames.each_with_index do |frame, idx|
          print_frame(frame, idx, cur_idx)
        end
      end
    end

    def print_frame(frame, idx, cur_idx)
      print "<frame no=\"%s\" file=\"%s\" line=\"%s\" #{'current="true" ' if idx == cur_idx}/>\n",
        idx+1, frame.file, frame.line
    end
    
    def print_contexts(contexts)
      print_element("threads") do
        contexts.each do |c|
          print_context(c)
        end
      end
    end

    def print_context(context)
      current = 'current="yes"' if context.thread == Thread.current
      print "<thread id=\"%s\" status=\"%s\" #{current}/>\n", context.thnum, context.thread.status
    end

    def print_variables(vars, binding, kind)
      print_element("variables") do
        # print self at top position
        if kind == "local" && eval('self.to_s', binding) !~ /main/ then
          print_variable("self", binding, kind)
        end
        vars.sort.each do |v|
          print_variable(v, binding, kind)
        end
      end
    end

    def print_variable(name, binding, kind)
      value = eval(name, binding)
      if value == nil then
        print("<variable name=\"%s\" kind=\"%s\"/>\n", CGI.escapeHTML(name), kind)
        return
      end
      if Array === value || Hash === value
        has_children = !value.empty?
        unless has_children
          value_str = "Empty #{value.class}"
        else
          value_str = "#{value.class} (#{value.size} elements(s))"
        end
      else
        has_children = !value.instance_variables.empty? || !value.class.class_variables.empty?
        value_str = value.to_s
        if value_str =~ /^\"(.*)"$/
          value_str = $1
        end
      end
      print("<variable name=\"%s\" kind=\"%s\" value=\"%s\" type=\"%s\" hasChildren=\"%s\" objectId=\"%#+x\"/>\n",
        CGI.escapeHTML(name), kind, CGI.escapeHTML(value_str), value.class, has_children, value.object_id)
    end

    def print_breakpoints(breakpoints)
      print_element 'breakpoints' do
        breakpoints.each_with_index do |b, n|
          print "<breakpoint n=\"%d\" file=\"%s\" line=\"%s\" />\n", n+1, b.source, b.pos.to_s
        end
      end
    end
    
    def print_expressions(exps)
      print_element "expressions" do
        exps.each_with_index do |(exp, value), idx|
          print_expression(exp, value, idx+1)
        end
      end unless exps.empty?
    end
    
    def print_expression(exp, value, idx)
      print "<dispay name=\"%s\" value=\"%s\" no=\"%d\" />\n", exp, value, idx
    end
    
    def print_eval(exp, value)
      print "<eval name=\"%s\" value=\"%s\" />\n", exp, value
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
      print_element "methods" do
        methods.each do |method|
          print "<method name=\"%s\" />\n", method
        end
      end
    end
    
    # Events
    
    def print_breakpoint(n, breakpoint)
      print("<breakpoint file=\"%s\" line=\"%s\" threadId=\"%d\"/>\n", 
        breakpoint.source, breakpoint.pos, Debugger.current_context.thnum)
    end

    def print_catchpoint(exception)
      context = Debugger.current_context
      frame = context.frames.first
      print("<exception file=\"%s\" line=\"%s\" type=\"%s\" message=\"%s\" threadId=\"%d\"/>\n", 
        frame.file, frame.line, exception.class, CGI.escapeHTML(exception.to_s), context.thnum)
    end

    def print_trace(context, file, line)
      print "<trace file=\"%s\" line=\"%s\" threadId=\"%d\" />\n", file, line, context.thnum
    end
    
    def print_at_line(file, line)
      print "<suspended file=\"%s\" line=\"%s\" threadId=\"%d\"/>\n", 
        file, line, Debugger.current_context.thnum
    end
    
    def print_exception(excpt, binding)
      print_catchpoint excpt
    end

    private

    def print(*params)
      @interface.print(*params)
    end

    def print_element(name)
      print("<#{name}>\n")
      begin
        yield
      ensure
        print("</#{name}>\n")
      end
    end
  end
end