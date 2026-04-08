// ORMDSL-Gurobi Build Configuration
// Compile with: scalac -classpath gurobipy.jar src/*.scala

name := "ormdsl-gurobi"

version := "0.1.0"

scalaVersion := "2.13.12"

// Dependencies
libraryDependencies ++= Seq(
  // Gurobi Java bindings (if available)
  // "com.gurobi" % "gurobi" % "10.0.0"
)

// Compiler options
scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked"
)

// Output directory
Compile / sourceManaged := file("target/generated-sources")
