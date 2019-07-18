name := "build_client"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "com.google.code.gson" % "gson" % "2.3.1"

libraryDependencies +=  "ch.epfl.scala" % "bsp4j" % "2.0.0-M4"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.12.5"

libraryDependencies += "com.geirsson" %% "coursier-small" % "1.3.3"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.9"

libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.2.6"

libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.7.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

libraryDependencies += "com.github.scopt" %% "scopt" % "4.0.0-RC2"
