ALL_TEST_FILES = FileList['test/test*.rb']

# TODO: describe why below are excluded
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

STABLE_TEST_FILES = ALL_TEST_FILES - UNSTABLE_TEST_FILES

desc "Test passing with jruby-debug-base."
task :test_stable => :test_base do
  Rake::TestTask.new(:test_stable) do |t|
    t.libs << './ext'
    t.libs << './lib'
    t.libs << './cli'
    t.test_files = STABLE_TEST_FILES
    t.verbose = true
  end
end
