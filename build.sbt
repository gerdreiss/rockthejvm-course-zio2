val scala3Version = "3.1.3"
val zio2Version   = "2.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "rockthejvm-course-zio2",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"            % zio2Version,
      "dev.zio" %% "zio-streams"    % zio2Version,
      "dev.zio" %% "zio-nio"        % zio2Version,
      "dev.zio" %% "zio-prelude"    % "1.0.0-RC15",
      "dev.zio" %% "zio-test"       % zio2Version,
      "dev.zio" %% "zio-test-junit" % zio2Version,
      "dev.zio" %% "zio-test-sbt"   % zio2Version
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZioFramework")
  )
