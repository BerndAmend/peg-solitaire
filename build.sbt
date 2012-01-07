import AssemblyKeys._ // put this at the top of the file

seq(assemblySettings: _*)

name := "peg-solitaire"

version := "0.4dev"

scalaVersion := "2.9.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.9.1"

scalacOptions ++= List("-unchecked", "-deprecation")

initialize ~= { _ => System.setProperty("peg-solitaire.usejavacp", "false") }
