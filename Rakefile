require 'rake/clean'
require 'rake/testtask'
require 'rdoc/task'
require 'rubygems/package_task'
require 'rake/javaextensiontask'

GEM_VERSION = '0.10.4'

CLEAN.include('ext')
CLEAN.include('lib/ruby_debug.jar')

BASE_TEST_FILE_LIST = %w(
  test/base/base.rb
  test/base/binding.rb 
  test/base/catchpoint.rb)

task :default => :package

CLI_TEST_FILE_LIST = 'test/test-*.rb'

desc "Test ruby-debug-base."
task :test_base => :lib do
  Rake::TestTask.new(:test_base) do |t|
    t.libs << './ext'
    t.libs << './lib'
    t.test_files = FileList[BASE_TEST_FILE_LIST]
    t.verbose = true
  end
end

desc "Test everything."
task :test => :test_base do 
  Rake::TestTask.new(:test) do |t|
    t.libs << './ext'
    t.libs << './lib'
    t.libs << './cli'
    t.pattern = CLI_TEST_FILE_LIST
    t.verbose = true
  end
end

desc "Helps to setup the project to be able to run tests"
task :prepare_tests do
  # needed to run CLI test. Unable to use svn:externals yet:
  #   http://subversion.tigris.org/issues/show_bug.cgi?id=937
  
  # rdbg.rb
  sh "svn cat svn://rubyforge.org/var/svn/ruby-debug/tags/ruby-debug-#{GEM_VERSION}/rdbg.rb > rdbg.rb" unless File.exists?('rdbg.rb')

  # runner.sh
  runner = 'runner.sh'
  sh "svn cat svn://rubyforge.org/var/svn/ruby-debug/tags/ruby-debug-#{GEM_VERSION}/runner.sh > #{runner}" unless File.exists?(runner)
  text = File.read('runner.sh')
  File.open(runner, 'w') {|f| f.write(text.gsub(/-ruby/ , '-jruby --debug'))}
  File.chmod(0755, runner)

  File.open('test/config.private.yaml', 'w') do |f|
    f.write <<EOF
# either should be on the $PATH or use full path
ruby: jruby

# possibility to specify interpreter parameters
ruby_params: --debug
EOF
  end

  # - prepare default customized test/config.private.yaml suitable for JRuby
  # - tweak test suite to be able to pass for jruby-debug-base which does not
  #   support e.g. TraceLineNumbers yet.
  sh "patch -p0 < patch-#{GEM_VERSION}.diff"
end

spec = Gem::Specification.new do |s|
  s.platform = "java"
  s.summary  = "Java implementation of Fast Ruby Debugger"
  s.name     = 'ruby-debug-base'
  s.version  = GEM_VERSION
  s.require_path = 'lib'
  s.files    = ['AUTHORS',
                'ChangeLog',
                'lib/linecache.rb',
                'lib/linecache-ruby.rb',
                'lib/ruby-debug-base.rb',
                'lib/ruby_debug.jar',
                'lib/tracelines.rb',
                'MIT-LICENSE',
                'Rakefile',
                'README']
  s.description = <<-EOF
Java extension to make fast ruby debugger run on JRuby.
It is the same what ruby-debug-base is for native Ruby.
EOF
  s.author   = 'debug-commons team'
  s.homepage = 'http://rubyforge.org/projects/debug-commons/'
  s.has_rdoc = true
  s.rubyforge_project = 'debug-commons'
end

Gem::PackageTask.new(spec) {}

Rake::JavaExtensionTask.new('ruby_debug') do |t|
  t.ext_dir = "src"
end

RDoc::Task.new do |t|
  t.main = 'README'
  t.rdoc_files.include 'README'
end


desc "Create a GNU-style ChangeLog via svn2cl"
task :ChangeLog do
  system("svn2cl --authors=svn2cl_usermap svn://rubyforge.org/var/svn/debug-commons/jruby-debug/trunk")
end
