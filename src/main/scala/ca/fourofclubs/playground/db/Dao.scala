package ca.fourofclubs.playground.db

import org.jooq.impl.DSL._
import org.jooq.RecordMapper
import org.jooq.Record
import org.jooq.impl.SQLDataType
import java.math.BigDecimal
import org.jooq.Record2

object UserDao {
  private val USERS = table("USERS")
  private val USERID = field("USERID", SQLDataType.NUMERIC)
  private val USERNAME = field("USERNAME", SQLDataType.VARCHAR)
  private def toUser = (r: Record2[BigDecimal, String]) => User(r.value1.intValue, r.value2)
  def apply(id: Int): Query[Option[User]] = dataSource => {
    val connection = dataSource.getConnection
    try {
      Option((using(connection)
        select (USERID, USERNAME)
        from USERS
        where USERID.eq(BigDecimal valueOf id)).fetchOne).map(toUser)
    } finally { connection.close }
  }
  def apply(username: String): Query[Option[User]] = dataSource => {
    val connection = dataSource.getConnection
    try {
      Option((using(connection)
        select (USERID, USERNAME)
        from USERS
        where USERNAME.eq(username)).fetchOne).map(toUser)
    } finally { connection.close }
  }
}

case class User(id: Int, username: String)