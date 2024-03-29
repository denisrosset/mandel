import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

val scalaFXVersion = "12.0.1-R17"
val scalaJSDomVersion = "0.9.7"
val miniSpireVersion = "1.0.5-SNAPSHOT"

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")

lazy val mandel = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .crossType(CrossType.Full)
  .in(file("."))
  .settings(
    resolvers ++= Seq(
      "bintray/denisrosset/maven" at "https://dl.bintray.com/denisrosset/maven"
    ),
    libraryDependencies += "org.typelevel" %%% "minispire" % "1.0.5-SNAPSHOT"
  )
  .jvmSettings(
    scalaVersion := "2.12.8",
    fork in run := true,
    libraryDependencies ++= javaFXModules.map( m =>
      "org.openjfx" % s"javafx-$m" % "12.0.1" classifier osName
    ),
    libraryDependencies += "org.scalafx" %% "scalafx" % scalaFXVersion,
  )
  .jsSettings(
    scalaVersion := "2.12.8",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % scalaJSDomVersion,
    scalaJSUseMainModuleInitializer := true
  )
  .nativeSettings(
    scalaVersion := "2.11.12"
  )


lazy val mandelJVM = mandel.jvm
lazy val mandelJS = mandel.js
lazy val mandelNative = mandel.native
