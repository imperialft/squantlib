package net.squantlib.util

import io.airbrake.javabrake._
import javax.net.ssl._
import java.security.cert._

case class Airbrake(
  projectId:Int,
  projectKey:String
) {

  // val notifier = new Notifier(projectId, projectKey)
  val config = new Config()
  config.projectId = projectId
  config.projectKey = projectKey
  val notifier = new Notifier(config)

  // println(s"initialize airbrake ${projectId} ${projectKey}")

  notifier.addFilter(
    (notice: Notice) => {
      notice.setContext("environment", "production")
      notice
    }
  )

  notifier.onReportedNotice(
    (notice: Notice) => {
      if (notice.exception != null) {
        println(notice.exception);
      } else {
        println(String.format("notice id=%s url=%s", notice.id, notice.url));
      }
    });  

  def report(e:Throwable) = {
    notifier.report(e)
  }

}

object Airbrake {

  var notifier:Option[Airbrake] = None

  def initialize(projectId:Int, projectKey:String) = {
    notifier = Some(Airbrake(projectId, projectKey))
  }

  def notify(e:Throwable):Boolean = notifier match {
    case Some(n) =>
      println("report Airbrake")
      n.report(e)
      true
    case _ =>
      false
  }

  val nullTrustManager = new X509TrustManager() {
    def checkClientTrusted(chain: Array[X509Certificate], authType: String): Unit = {
      //do nothing, you're the client
    }

    def getAcceptedIssuers():Array[X509Certificate] = {
      //also only relevant for servers
      Array()
    }

    def checkServerTrusted(chain: Array[X509Certificate], authType: String): Unit = {
      /* chain[chain.length -1] is the candidate for the
        * root certificate. 
        * Look it up to see whether it's in your list.
        * If not, ask the user for permission to add it.
        * If not granted, reject.
        * Validate the chain using CertPathValidator and 
        * your list of trusted roots.
        */
    }
  };    

}
