name := "scala-sift"

organization := "com.ploomes"

version := "1.0"

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.12.1" ,"2.11.7")

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "org.scala-lang" % "scala-compiler" % "2.12.1" exclude("org.scala-lang.modules", "scala-xml_2.12"),
  "org.scala-lang" % "scala-library" % "2.12.1" exclude("org.scala-lang.modules", "scala-xml_2.12"),
  "org.scala-lang" % "scala-reflect" % "2.12.1" exclude("org.scala-lang.modules", "scala-xml_2.12")
)

fork in run := true
    