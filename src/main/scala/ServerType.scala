package laughedelic.atom.ide.scala

import scala.scalajs.js, js.annotation._, js.Dynamic.global
import scala.concurrent._

import fr.hmil.roshttp.HttpRequest
import fr.hmil.roshttp.response.SimpleHttpResponse
import upickle.default._

sealed trait ServerType {

  val name: String

  def javaArgs(projectPath: String): Seq[String]
  def coursierArgs(version: String)(implicit ec: ExecutionContext): Future[Seq[String]]

  def watchFilter(filePath: String): Boolean
}

case class BintrayPackageVersion(
  name: String,
  `package`: String,
  repo: String,
  owner: String,
  created: String,
  updated: String
)

object BintrayPackageVersion{
  implicit def rw: ReadWriter[BintrayPackageVersion] = macroRW
}

case object ServerType {

  case object Scalameta extends ServerType {
    val name: String = "Scalameta"

    private def getLastestVersion: Future[String] = {
      import monix.execution.Scheduler.Implicits.global

      val request = HttpRequest("https://bintray.com/api/v1/packages/scalameta/maven/metaserver/versions/_latest")
      request.send().map(res => read[BintrayPackageVersion](res.body).name)
    }
    
    def javaArgs(projectPath: String): Seq[String] = Seq(
      // "-XX:+UseG1GC",
      // "-XX:+UseStringDeduplication"
    )

    def coursierArgs(version: String = "0.1-SNAPSHOT")(implicit ec: ExecutionContext): Future[Seq[String]] =
      Future
        .successful(version)
        .filter(_ != "lastest")
        .fallbackTo(getLastestVersion)
        .map { version =>
          Seq(
            "--repository", "bintray:dhpcs/maven",
            "--repository", "bintray:scalameta/maven",
            s"org.scalameta:metaserver_2.12:${version}",
            "--main", "scala.meta.languageserver.Main"
          )
        }

    def watchFilter(filePath: String): Boolean = {
      filePath.endsWith(".semanticdb") ||
      filePath.endsWith(".compilerconfig")
    }
  }

  case object Ensime extends ServerType {
    val name: String = "ENSIME"

    def javaArgs(projectPath: String): Seq[String] = Seq(
      "-Xmx4G", // heap size
      // FIXME: how to setup classpath properly without parsing .ensime config?
      // "-classpath", classpath,
      s"-Dlsp.workspace=${projectPath}",
      // TODO: add log level to the plugin settings
      s"-Dlsp.logLevel=DEBUG",
    )

    def coursierArgs(version: String = "3.0.0-SNAPSHOT")(implicit ec: ExecutionContext): Future[Seq[String]] =
      Future.successful {
        Seq(
          "--repository", "bintray:dhpcs/maven",
          "--repository", "sonatype:snapshots",
          s"org.ensime:server_2.12:${version}",
          "--main", "org.ensime.server.Server",
          "--", "--lsp"
        )
      }

    def watchFilter(filePath: String): Boolean = {
      // TODO: should be more precise:
      filePath.contains(".ensime")
    }
  }

  def fromConfig: ServerType = {
    global.atom.config.get("ide-scala.serverType").toString match {
      case "scalameta" => Scalameta
      case "ensime" => Ensime
    }
  }
}
