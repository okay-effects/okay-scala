// okay-scala: algebraic effects for Scala 3 alone — rows `!`, `+`, `%` — and one machine (CEK). Self-contained: no
// dependency. okay-simple's Scala 3 build takes its rows from here, and runs its interface on this machine
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

/** the library is built by Scala 3.3 LTS, so every Scala 3 since reads it; its tests run on the newest too */
val scala3 = "3.3.8"
val scala3Next = "3.9.0"

ThisBuild / organization := "io.github.okay-effects"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala3
ThisBuild / crossScalaVersions := Seq(scala3, scala3Next)
ThisBuild / scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Werror",
  "-Wunused:imports,privates,locals,implicits")
ThisBuild / licenses := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/okay-effects/okay-scala"))
ThisBuild / developers := List(Developer("sergey-scherbina", "Sergiy Shcherbyna", "sergey.scherbina@gmail.com",
  url("https://github.com/sergey-scherbina")))

/** the platforms built: the JVM always, Scala.js and Scala Native when asked — `sbt -Dplatforms=jvm,js,native` */
val platformsAsked: Set[String] =
  sys.props.get("platforms").getOrElse("jvm").split(',').map(_.trim.toLowerCase).toSet + "jvm"
val platforms: Seq[sbtcrossproject.Platform] = Seq[sbtcrossproject.Platform](JVMPlatform) ++
  (if (platformsAsked("js")) Seq(JSPlatform) else Nil) ++ (if (platformsAsked("native")) Seq(NativePlatform) else Nil)

/** THE LIBRARY: rows, programs, handlers, the CEK machine, the standard effects */
lazy val core = crossProject(platforms: _*).crossType(CrossType.Pure).in(file("core"))
  .settings(name := "okay-scala", libraryDependencies += "org.scalameta" %%% "munit" % "1.2.3" % Test)

/** the machine measured (JMH) */
lazy val bench = (project in file("bench")).dependsOn(core.jvm).enablePlugins(JmhPlugin)
  .settings(name := "okay-scala-bench", publish / skip := true)

lazy val root = (project in file(".")).aggregate((core.componentProjects.map(p => p: sbt.ProjectReference) :+
  (bench: sbt.ProjectReference)): _*).settings(publish / skip := true)
