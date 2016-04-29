package ca.fourofclubs.playground

import javax.sql.DataSource
package object db {
  type Query[A] = DataSource => A
  type Process = DataSource => Unit
}