/**
  * Created by rafaeleal on 2/11/17.
  */
import org.scalatest._
import sift.Sift

class SiftSpec extends FlatSpec {
  "$in" should "return true when nested" in {
    val craig: Map[String, Any] = Map("name" -> "Craig", "location" -> "Brazil")
    val query: Boolean = Sift(Map("location" -> Map("$in" -> List("Costa Rica", "Brazil")))).testQuery(craig)
    //    assert(query)
  }
  "$eq" should "return even when ommited" in {
    val query: Boolean = Sift(Map("Name" -> "José")).testQuery(Map[String, Any]("Name" -> "José", "CPF" -> "2324242"))
    assert(query)
  }
  it should "return when is explicit passed" in {
    val query: Boolean = Sift(Map("Name" -> Map("$eq"-> "José"))).testQuery(Map[String, Any]("Name" -> "José", "CPF" -> "2324242"))
    assert(query)
  }
  "$ne" should "return true when is not equal" in {
    val query: Boolean = Sift(Map("Name" -> Map("$ne"-> "José"))).testQuery(Map[String, Any]("Name" -> "José", "CPF" -> "2324242"))
    assert(!query)
  }
  "$gte" should "return true when equal" in {
    val query: Boolean = Sift(Map[String, Any]("$gte" -> 3)).testQuery(3)
    assert(query)
  }
  it should "return true when greater" in {
    val query: Boolean = Sift(Map[String, Any]("$gte" -> 3)).testQuery(4)
    assert(query)
  }
  it should "return false when less" in {
    val query: Boolean = Sift(Map[String, Any]("$gte" -> 3)).testQuery(2)
    assert(!query)
  }

  "$gt" should "return false when equal" in {
    val query: Boolean = Sift(Map[String, Any]("$gt" -> 3)).testQuery(3)
    assert(!query)
  }
  it should "return true when greater" in {
    val query: Boolean = Sift(Map[String, Any]("$gt" -> 3)).testQuery(4)
    assert(query)
  }
  it should "return false when less" in {
    val query: Boolean = Sift(Map[String, Any]("$gt" -> 3)).testQuery(2)
    assert(!query)
  }

  "$lte" should "return true when equal" in {
    val query: Boolean = Sift(Map[String, Any]("$lte" -> 3)).testQuery(3)
    assert(query)
  }
  it should "return false when greater" in {
    val query: Boolean = Sift(Map[String, Any]("$lte" -> 3)).testQuery(4)
    assert(!query)
  }
  it should "return true when less" in {
    val query: Boolean = Sift(Map[String, Any]("$lte" -> 3)).testQuery(2)
    assert(query)
  }

  "$lt" should "return false when equal" in {
    val query: Boolean = Sift(Map[String, Any]("$lt" -> 3)).testQuery(3)
    assert(!query)
  }
  it should "return false when greater" in {
    val query: Boolean = Sift(Map[String, Any]("$lt" -> 3)).testQuery(4)
    assert(!query)
  }
  it should "return true when less" in {
    val query: Boolean = Sift(Map[String, Any]("$lt" -> 3)).testQuery(2)
    assert(query)
  }

  "$and" should "return true when all conditions are met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 3, "Nome" -> "Carlos")
    assert(Sift(Map("$and" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }
  it should "return false when some condition are not met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlos")
    assert(!Sift(Map("$and" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }
  it should "return false when all conditions are not met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlinhos")
    assert(!Sift(Map("$and" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }

  "$nand" should "return false when all conditions are met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 3, "Nome" -> "Carlos")
    assert(!Sift(Map("$nand" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }
  it should "return true when some condition are not met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlos")
    assert(Sift(Map("$nand" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }
  it should "return true when all conditions are not met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlinhos")
    assert(Sift(Map("$nand" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }

  "$or" should "return true when all conditions are met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 3, "Nome" -> "Carlos")
    assert(Sift(Map("$or" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }
  it should "return true when some condition are not met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlos")
    assert(Sift(Map("$or" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }
  it should "return false when all conditions are not met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlinhos")
    assert(!Sift(Map("$or" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }

  "$nor" should "return true when all conditions are met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 3, "Nome" -> "Carlos")
    assert(!Sift(Map("$nor" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }
  it should "return true when some condition are not met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlos")
    assert(!Sift(Map("$nor" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }
  it should "return false when all conditions are not met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlinhos")
    assert(Sift(Map("$nor" -> List(Map("Valor" -> 3), Map("Nome" -> "Carlos")))).testQuery(carlos))
  }

  "$not" should "return true when the condition is not met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlinhos")
    assert(Sift(Map("$not" -> Map("Valor" -> 3))).testQuery(carlos))
  }
  it should "return false when the condition is met" in {
    val carlos: Map[String, Any] = Map("Valor" -> 5, "Nome" -> "Carlinhos")
    assert(!Sift(Map("$not" -> Map("Valor" -> 5))).testQuery(carlos))
  }

  "$where" should "run scala code" in {
    val obj: Map[String, Any] = Map("Valor" -> 5, "Custo" -> 50)
    assert(Sift(Map("$where" -> """obj("Valor").asInstanceOf[Int] < obj("Custo").asInstanceOf[Int]""")).testQuery(obj))
  }
}
