package squantlib.database.schemadefinitions

trait DBEntity[T] {
  var id:T
}

trait StringEntity extends DBEntity[String] 

trait IntEntity extends DBEntity[Int] 

