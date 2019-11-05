package net.squantlib.util

import io.airbrake.javabrake._


case class Airbrake(
  projectId:Int,
  projectKey:String
) {

  val notifier = new Notifier(projectId, projectKey)

  notifier.addFilter(
    (notice: Notice) => {
      notice.setContext("environment", "production")
      notice
    }
  )

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
      n.report(e)
      true
    case _ =>
      false
  }

}