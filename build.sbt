
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val lofi = (project in file("."))
  //.settings(ScalaModulePlugin.scalaModuleSettings)
  //.settings(ScalaModulePlugin.scalaModuleOsgiSettings)
  .settings(
    name := "lofi",
    ThisBuild / shellPrompt := { state => Project.extract(state).currentRef.project + "> " },
    resolvers += Resolver.url("Mvn Repository", url("https://mvnrepository.com/artifact/"))(Resolver.ivyStylePatterns),
    libraryDependencies ++= Seq(
      "org.apache.commons" % "commons-math3" % "3.3",
      "org.scala-lang.modules" %% "scala-swing" % "3.0.0",
      "com.formdev" % "flatlaf" % "2.0-rc1",
      "org.vaadin.addons" % "scaladin" % "3.1.0",
      "com.vaadin" % "vaadin-server" % "7.5.10",
      "com.vaadin" % "vaadin-client-compiled" % "7.5.10",
      "com.vaadin" % "vaadin-themes" % "7.5.10"
    )
  )

lazy val gui = project.in(file("gui"))
  .dependsOn(lofi)
  .settings(
    run / fork := true,
    fork := true,
  )
