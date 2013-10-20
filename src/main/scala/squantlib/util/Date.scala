package squantlib.util

import java.util.{Date => JavaDate}
import org.jquantlib.time.{Date => qlDate}

trait Date {
  def java:JavaDate
  def ql:qlDate
  def ge(d:Date):Boolean = ql ge d.ql
  def le(d:Date):Boolean = ql le d.ql
  def gt(d:Date):Boolean = ql gt d.ql
  def lt(d:Date):Boolean = ql lt d.ql
}

object Date {
  def apply(d:JavaDate):Date = new JavaDateImpl(d)
  def apply(d:qlDate):Date = new QlDateImpl(d)
  
}

class JavaDateImpl(d:JavaDate) extends Date{
  lazy val qldate = new qlDate(d)
  override def java:JavaDate = d
  override def ql:qlDate = qldate
}

class QlDateImpl(d:qlDate) extends Date{
  lazy val javadate = d.longDate
  override def java = javadate
  override def ql = d
}