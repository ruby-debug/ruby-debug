STABLE_TEST_FILES = %w(
  test/test-annotate.rb
  test/test-break-bad.rb
  test/test-breakpoints.rb
  test/test-condition.rb
  test/test-ctrl.rb
  test/test-display.rb
  test/test-emacs-basic.rb
  test/test-enable.rb
  test/test-frame.rb
  test/test-help.rb
  test/test-info.rb
  test/test-info-thread.rb
  test/test-info-var.rb
  test/test-init.rb
  test/test-list.rb
  test/test-output.rb
  test/test-quit.rb
  test/test-setshow.rb
  test/test-source.rb
  test/test-stepping.rb
  test/test-hist.rb
)

UNSTABLE_TEST_FILES = %w(
  test/test-finish.rb
  test/test-pm.rb
  test/test-trace.rb
)

# Does not pass due to:
# [#JRUBY-2816] Kernel#system call from within debugger session freezes JRuby
#   - http://jira.codehaus.org/browse/JRUBY-2816
UNSTABLE_TEST_FILES << 'test/test-edit.rb'

# Does not pass, because exception is written to stderr instead of stdout as in
# MRI. Investigate.
UNSTABLE_TEST_FILES << 'test/test-raise.rb'

# Depends on:
# [#JRUBY-2456] Broken tracing for Kernel.load
#   - http://jira.codehaus.org/browse/JRUBY-2456
UNSTABLE_TEST_FILES << 'test/test-dollar-0.rb'

desc "Test passing with jruby-debug-base."
task :test_stable => :test_base do 
  Rake::TestTask.new(:test_stable) do |t|
    t.libs << ['./ext', './lib', './cli']
    t.test_files = STABLE_TEST_FILES
    t.verbose = true
  end
end
