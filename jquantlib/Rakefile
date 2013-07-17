def java_archives
  @java_archives ||= Dir.glob("lib/**/*.jar")
end

def vendor_java_archives
  @vendor_java_archives ||= Dir.glob("vendor/**/*.jar")
end

def java_source_directories
  @java_source_directories ||= Dir.glob("*/src/main/java")
end

def java_sources
  return @java_sources if @java_sources
  @java_sources = []
  java_source_directories.each do |dir|
    Dir.glob("#{dir}/**/*.java").each do |java|
      @java_sources << java
    end
  end
  @java_sources
end

def java_classes
  @java_classes ||= Dir.glob(File.join(bin_directory, "**/*.class"))
end

def bin_directory
  return @bin_directory if @bin_directory
  @bin_directory = "tmp/bin"
  Dir.mkdir @bin_directory unless File.exists?(@bin_directory)
  @bin_directory
end

def packages
  @packages ||= Dir.glob("pkg/**/*.jar").sort
end

def latest_package
  @latest_package ||= packages.last
end

desc "Compile sources"
task :compile do
  system "javac -classpath #{java_archives.join(':')} -sourcepath #{java_source_directories.join(':')} -d #{bin_directory} #{java_sources.join(' ')}"
end

desc "Package *.jar file"
task :package => :compile do
  jar = File.join("jquantlib-#{Time.now.to_s.scan(/\d+/).join}.jar")
  system "jar cf #{jar} -C #{bin_directory} ."
end

desc "Open console (Scala REPL)"
task :repl do
  system "scala -classpath #{java_archives.join(':')}:#{latest_package}"
end

namespace :spec do
  def scala_spec_sources
    @scala_spec_sources ||= Dir.glob("spec/**/*Spec.scala")
  end
  def scala_spec_bin_directory
    return @scala_spec_bin_directory if @scala_spec_bin_directory
    @scala_spec_bin_directory = "tmp/spec_bin"
    Dir.mkdir @scala_spec_bin_directory unless File.exists?(@scala_spec_bin_directory)
    @scala_spec_bin_directory
  end
  def scala_spec_classes
    @scala_spec_classes ||= scala_spec_sources.map do |scala|
      File.read(scala).scan(/class [a-zA-Z_]+/).map do |clazz|
        clazz.split(" ").last
      end
    end.flatten.uniq.sort
  end
  desc "Compile specs"
  task :compile do
    system "scalac -classpath #{(java_archives + vendor_java_archives).join(':')}:#{bin_directory} -d #{scala_spec_bin_directory} #{scala_spec_sources.join(' ')}"
  end
  desc "Clean compiled specs"
  task :clean do
    require "fileutils"
    FileUtils.rm_rf scala_spec_bin_directory
  end
  desc "Run specs"
  task :run do
    at_exit do
      require "fileutils"
      FileUtils.rm_rf "target"
    end
    scala_spec_classes.each do |clazz|
      system "scala -J-Xss200m -classpath #{(java_archives + vendor_java_archives).join(':')}:#{bin_directory}:#{scala_spec_bin_directory} specs2.run #{clazz}"
    end
  end
end

desc "Run specs (alias to spec:run)"
task :spec => :"spec:run"