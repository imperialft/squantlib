def scala_sources
  @scala_sources ||= Dir.glob("{squantlib,squantlib-test}/src/**/*.scala")
end

def java_archives
  @java_archives ||= Dir.glob("lib/**/*.jar")
end

def class_paths
  @class_paths ||= %w(bin ../jquantlib/tmp/bin).reject { |dir| !File.exists?(dir) || !File.directory?(dir) }
end

def unit_test_classes
  @unit_test_classes ||= Dir.glob("squantlib-test/src/squantlib/test/**/*Test.scala").map { |scala| File.read(scala).scan(/class\s+\w+/).map { |s| s.split(/\s+/).last } }.flatten.uniq.map { |clazz| "org.squantlib.test.#{clazz}" }
end

task :compile do
  system "scalac -classpath #{(java_archives | class_paths).join(":")} -d bin #{scala_sources.join(" ")}"
end

task :test do
  system "scala -classpath #{(java_archives | class_paths).join(":")} org.junit.runner.JUnitCore " + unit_test_classes.join(" ")
end
