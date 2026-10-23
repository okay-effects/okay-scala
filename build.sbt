// okay-scala: algebraic effects for Scala 3 alone — rows `!`, `+`, `%` — and one machine (CEK). Self-contained: no
// dependency. okay-simple's Scala 3 build takes its rows from here, and runs its interface on this machine
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

/** the library is built by Scala 3.3 LTS, so every Scala 3 since reads it; its tests run on the newest too */
val scala3 = "3.3.8"
val scala3Next = "3.9.0"

ThisBuild / organization := "io.github.okay-effects"
ThisBuild / organizationName := "okay-effects"
ThisBuild / version := "0.1.1-SNAPSHOT"
ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/okay-effects/okay-scala"),
  "scm:git:https://github.com/okay-effects/okay-scala.git"))
ThisBuild / startYear := Some(2026)
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / description := "Algebraic effects for Scala 3 alone: rows A ! R, R + E, F % S; one CEK machine, enums and GADTs, no cast"

/** RELEASES, ON GITHUB PAGES: `sbt -Dplatforms=jvm,js,native publish` writes the library for all three platforms, as a
 * Maven repository, into `target/pages-maven/`; that goes to the `gh-pages` branch under `maven/`, served at
 * https://okay-effects.github.io/okay-scala/maven (RELEASING.md). Without `+`: the `_3` artifacts are Scala 3.3's */
val pagesRepository = settingKey[File]("the Maven repository a release is written to, for GitHub Pages")
ThisBuild / pagesRepository := (ThisBuild / baseDirectory).value / "target" / "pages-maven"
ThisBuild / publishTo := Some(MavenCache("pages", (ThisBuild / pagesRepository).value))
/** no API documentation jars, as okay-simple: the sources jars are published, which IDEs read */
ThisBuild / Compile / packageDoc / publishArtifact := false
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
