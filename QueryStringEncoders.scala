
object QueryStringEncoders {

  trait QueryString[T] {
    def queryString(t: T): String
  }

  implicit object QueryStringString extends QueryString[String] {
    override def queryString(s: String): String = s
  }

  implicit object QueryStringLong extends QueryString[Long] {
    override def queryString(t: Long): String = t.toString
  }

  implicit object QueryStringInteger extends QueryString[Integer] {
    override def queryString(t: Integer): String = t.toString
  }

  implicit object QueryStringFloat extends QueryString[Float] {
    override def queryString(t: Float): String = t.toString
  }

  implicit object QueryStringBoolean extends QueryString[Boolean] {
    override def queryString(t: Boolean): String = if (t) "true" else "false"
  }


  implicit class QueryStringableObject[T](t: T) {
    def queryString(implicit qsi: QueryString[T]): String = qsi.queryString(t)
  }

  implicit def listQueryString[T](implicit q: QueryString[T]): QueryString[List[T]] = new QueryString[List[T]] {
    override def queryString(xs: List[T]): String = xs.foldLeft("") { (a, n) =>
      if (a == "") n.queryString else a + "," + n.queryString
    }
  }

}
