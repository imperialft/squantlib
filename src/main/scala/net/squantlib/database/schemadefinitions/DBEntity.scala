package net.squantlib.database.schemadefinitions

import java.sql.Timestamp

trait DBEntity[T] {
  var id:T
  var created: Timestamp
  var lastmodified: Timestamp
}

trait StringEntity extends DBEntity[String] 

trait IntEntity extends DBEntity[Int] 

