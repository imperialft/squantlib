package squantlib.database.schemadefinitions

import org.squeryl.dsl.ast.LogicalBoolean  

trait DBEntity[T] {
  var id:T
}

trait StringEntity extends DBEntity[String] 

trait IntEntity extends DBEntity[Int] 

