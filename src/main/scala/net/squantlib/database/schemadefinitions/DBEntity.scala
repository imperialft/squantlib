package net.squantlib.database.schemadefinitions

import java.sql.Timestamp

trait DBEntity[T] {
  var id:T
}

trait TimestampEntity {
  var created: Timestamp
  var lastmodified: Timestamp
}

trait StringEntity extends DBEntity[String] with TimestampEntity

trait IntEntity extends DBEntity[Int] with TimestampEntity

