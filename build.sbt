logLevel := Level.Debug

lazy val root = project.in(file("."))
  .settings(
    organization  :=  "org.selwyn",
    name          :=  "problems",
    version       :=  "0.0.1",
    scalaVersion  :=  "2.12.4",
    description   :=  "Scala problems",
    scalacOptions :=  BuildSettings.compilerOptions,
    scalacOptions in  (Compile, console) ~= { _.filterNot(Set("-Ywarn-unused-import")) },
    scalacOptions in  (Test, console)    := (scalacOptions in (Compile, console)).value,
    javacOptions  :=  BuildSettings.javaCompilerOptions,
    resolvers     ++= Dependencies.resolutionRepos,
    shellPrompt   :=  { _ => "problems> "}
  )
  .settings(BuildSettings.wartremoverSettings)
  .settings(BuildSettings.scalaFmtSettings)
  .settings(BuildSettings.scalifySettings)
  .settings(BuildSettings.sbtAssemblySettings)
  .settings(
    libraryDependencies ++= Dependencies.Libraries
  )
