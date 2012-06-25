//package squantlib.database
//
//import org.squeryl.PrimitiveTypeMode._
//
//object CLI {
//  def main(args:Array[String]):Unit = {
//    if (args.length <= 0) {
//      help(args)
//    } else {
//      try {
//        this.getClass.getMethod(args(0), args.getClass()).invoke(this, args)
//      } catch {
//        case e:NoSuchMethodException => println("Task '" + args(0) + "' is not defined.")
//        case e:Exception => throw(e)
//      }
//    }
//  } 
//  private def initialize():Unit = {
//    Core.noop()
//  }
//  protected def help(args:Array[String]):Unit = {
//    initialize()
//    println("Usage: FIXME")
//  }
//  protected def test(args:Array[String]):Unit = {
//    initialize()
//    Math.jquantLibTest()
//  }
//}