name := course.value + "-" + assignment.value

scalaVersion := "2.11.12"

scalacOptions ++= Seq("-deprecation")

// grading libraries
libraryDependencies += "junit" % "junit" % "4.10" % Test

// for funsets
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

// include the common dir
commonSourcePackages += "common"

courseId := "bRPXgjY9EeW6RApRXdjJPw"
commands += Command.command("cls") { state =>
  print("\033c") // clear char in xterm
  state
}

