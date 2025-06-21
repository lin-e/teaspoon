name := "preprocessor"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies += "com.github.j-mie6" %% "parsley" % "3.3.6"
libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.1"

assemblyJarName in assembly := "preprocessor.jar"
target in assembly := file("bin")
mainClass in assembly := Some("preprocessor.Main")
