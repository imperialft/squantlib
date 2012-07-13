SP = if RUBY_PLATFORM =~ /(mingw|mswin)/
  ";"
else
  ":"
end

def scala_sources
  @scala_sources ||= Dir.glob("{squantlib,squantlib-test}/src/**/*.scala")
end

def java_archives
  @java_archives ||= Dir.glob("{squantlib,squantlib-test}/lib/**/*.jar")
end

def class_paths
  @class_paths ||= %w(bin ../jquantlib/tmp/bin).reject { |dir| !File.exists?(dir) || !File.directory?(dir) }
end

def unit_test_classes
  @unit_test_classes ||= Dir.glob("squantlib-test/src/squantlib/test/**/*Test.scala").map { |scala| File.read(scala).scan(/class\s+\w+/).map { |s| s.split(/\s+/).last } }.flatten.uniq.map { |clazz| "squantlib.test.#{clazz}" }
end

task :compile, :a1 do |t, args|
  require 'fileutils'
  FileUtils.rm_rf 'bin'
  Dir.mkdir 'bin'
  sources = if args[:a1]
    scala_sources.select { |s| s =~ Regexp.new(args[:a1]) }
  else
    scala_sources
  end
  command = "scalac -classpath #{(java_archives | class_paths).join(SP)} -d bin #{sources.join(" ")}"
  File.open(".compiled", "wb"){} if system(command)
  if File.exists?(".compiled")
    File.delete(".compiled")
  else
    FileUtils.rm_rf 'bin'
  end
end

task :test do
  system "scala -classpath #{(java_archives | class_paths).join(SP)} org.junit.runner.JUnitCore " + unit_test_classes.join(" ")
end

task :default => :test

task :repl do
  system "scala -classpath #{(java_archives | class_paths).join(SP)}"  
end

task :cli do
  command = "scala -classpath #{(java_archives | class_paths).join(SP)} squantlib.database.CLI config_example.properties"
  puts command
  system command
end
