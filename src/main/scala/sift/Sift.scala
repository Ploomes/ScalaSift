package sift

import runner.Runner

/**
  * Created by rafaeleal on 2/11/17.
  */

case class Sift(query: Map[String, Any]) {
  private def toDouble(x: Any): Double = x match {
    case int: Int => int.toDouble
    case double: Double => double
    case float: Float => float.toDouble
    case _ => 0.0d
  }

  def testQuery(data: Any) : Boolean = {

    data match {
      case list: List[Map[String, Any]] =>
        query.keySet.forall({
          case "$in" =>
            val allowed = query("$in").asInstanceOf[List[Map[String, Any]]]
            list.exists(item => allowed.exists(q => Sift(q).testQuery(item)))
          case "$nin" =>
            val notAllowed = query("$nin").asInstanceOf[List[Map[String, Any]]]
            !list.exists(item => notAllowed.exists(q => Sift(q).testQuery(item)))
        })
      case map: Map[String, Any] =>
        query.keySet.forall({
          case "$and" =>
            val conditions: List[Map[_, _]] = query("$and").asInstanceOf[List[Map[_, _]]]
            conditions.forall(condition => Sift(condition.asInstanceOf[Map[String, Any]]).testQuery(data))
          case "$nand" =>
            val conditions: List[Map[_, _]] = query("$nand").asInstanceOf[List[Map[_, _]]]
            !conditions.forall(condition => Sift(condition.asInstanceOf[Map[String, Any]]).testQuery(data))
          case "$or" =>
            val conditions: List[Map[_, _]] = query("$or").asInstanceOf[List[Map[_, _]]]
            conditions.exists(condition => Sift(condition.asInstanceOf[Map[String, Any]]).testQuery(data))
          case "$nor" =>
            val conditions: List[Map[_, _]] = query("$nor").asInstanceOf[List[Map[_, _]]]
            !conditions.exists(condition => Sift(condition.asInstanceOf[Map[String, Any]]).testQuery(data))
          case "$not" =>
            val condition: Map[String, Any] = query("$not").asInstanceOf[Map[String, Any]]
            !Sift(condition).testQuery(data)
          case "$where" =>
            val code: String = query("$where").asInstanceOf[String]
            SiftMethods.where(code)(data.asInstanceOf[Map[String, Any]])
          case key: String =>
          query(key) match {
            case obj: Map[String, Any] =>
              map.get(key).exists(Sift(obj).testQuery)
            case any: Any =>
              map.get(key).exists(Sift(Map("$eq" -> any)).testQuery)
          }
        })
      case value @(_:String | _:Int | _:Boolean | _:Double | null) =>
        query.keySet.forall {
          case "$in" => SiftMethods.in(query("$in").asInstanceOf[List[Any]])(List(value))
          case "$nin" => SiftMethods.in(query("$nin").asInstanceOf[List[Any]])(List(value))
          case "$eq" => SiftMethods.eq(query("$eq"), value)
          case "$ne" => SiftMethods.ne(query("$ne"), value)
          case "$regex" => SiftMethods.regex(query("$regex").asInstanceOf[String], value.asInstanceOf[String])
          case "$exists" => SiftMethods.exists(query("$exists").asInstanceOf[Boolean], value)
          case "$gte" => SiftMethods.gte(toDouble(value), toDouble(query("$gte")))
          case "$gt" => SiftMethods.gt(toDouble(value), toDouble(query("$gt")))
          case "$lte" => SiftMethods.lte(toDouble(value), toDouble(query("$lte")))
          case "$lt" => SiftMethods.lt(toDouble(value), toDouble(query("$lt")))
          case "$mod" =>
            val divideBy = query("$mod").asInstanceOf[List[Int]].head
            val remainder = query("$mod").asInstanceOf[List[Int]].last
            SiftMethods.mod(divideBy, remainder)(value.asInstanceOf[Int])
          case "$all" => SiftMethods.all(query("$all").asInstanceOf[List[Any]])(value.asInstanceOf[List[Any]])
          case "$where" => SiftMethods.where(query("$where").asInstanceOf[String])(value.asInstanceOf[Map[String, Any]])
          case "$size" => SiftMethods.size(query("$size").asInstanceOf[Int])(value.asInstanceOf[List[Any]])
          case _ => false
        }
      case _ => false
    }
  }
}

object SiftMethods {
  def in(allowed: List[Any])(all: List[Any]): Boolean = all.exists(allowed.contains)
  def nin(notAllowed: List[Any])(all: List[Any]): Boolean = !in(notAllowed)(all)
  def eq(left: Any, right: Any): Boolean = left == right
  def ne(left: Any, right: Any): Boolean = !eq(left, right)
  def regex(regex: String, value: String): Boolean = regex.r.findAllIn(value).nonEmpty
  def exists(bool: Boolean, value: Any) = if(bool) value != null else value == null
  def gte(left: Double, right: Double) : Boolean = left >= right
  def gt(left: Double, right: Double) = left > right
  def lte(left: Double, right: Double) = left <= right
  def lt(left: Double, right: Double) = left < right
  def mod(divideBy: Int, remainder: Int)(value: Int) = (value % divideBy) == remainder
  def all(must: List[Any])(value: List[Any]) = must.forall(value.contains)
  def size(size: Int)(list: List[Any]) = list.length == size
  def where(expr: String)(obj: Map[String, Any]) : Boolean = Runner.evalExpression(obj)(expr).asInstanceOf[Boolean]
}
