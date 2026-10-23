// okay-scala: algebraic effects for Scala 3 alone — the rows of okay-simple (`!`, `+`, `%`), one machine (CEK)
val okaySimple = "0.1.1"

ThisBuild / organization := "io.github.okay-effects"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.9.0"
ThisBuild / resolvers += "okay-effects" at "https://okay-effects.github.io/okay-simple/maven"
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Werror", "-Wunused:imports,privates,locals,implicits")
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/okay-effects/okay-scala"))

val munit = "org.scalameta" %% "munit" % "1.2.3" % Test

/** THE INTERFACE AND ITS MACHINE: rows, programs, handlers, the CEK machine, the standard effects. Its rows are
 * okay-simple's own types (`okay.effects.Sum`, `Void`), so a row written here is the row written there */
lazy val core = (project in file("core"))
  .settings(name := "okay-scala",
    libraryDependencies ++= Seq("io.github.okay-effects" %% "okay-effects" % okaySimple, munit))

/** OKAY-SIMPLE'S INTERFACE ON THIS MACHINE: `okay.effects.Runner`, its programs this machine's — checked by
 * okay-simple's own suite */
lazy val simple = (project in file("simple")).dependsOn(core)
  .settings(name := "okay-scala-simple",
    libraryDependencies ++= Seq("io.github.okay-effects" %% "okay-effects" % okaySimple,
      "io.github.okay-effects" %% "okay-effects-tck" % okaySimple % Test, munit))

/** the machine measured against okay-simple's own (JMH) */
lazy val bench = (project in file("bench")).dependsOn(core, simple).enablePlugins(JmhPlugin)
  .settings(name := "okay-scala-bench", publish / skip := true,
    libraryDependencies ++= Seq("io.github.okay-effects" %% "okay-effects-machine" % okaySimple, munit))

lazy val root = (project in file(".")).aggregate(core, simple, bench).settings(publish / skip := true)
