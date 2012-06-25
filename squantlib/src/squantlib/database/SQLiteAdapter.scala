package squantlib.database

import java.sql.SQLException
import org.squeryl.internals.DatabaseAdapter

class SQLiteAdapter extends DatabaseAdapter {
  def isTableDoesNotExistException(e:SQLException):Boolean = e.getMessage.contains("table") && e.getMessage.contains("already exists")
  // FIXME: Date and DateTime not working!
}