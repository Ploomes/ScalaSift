import org.scalatest.FlatSpec
import sift.Sift

/**
  * Created by rafaeleal on 06/04/17.
  */
class SiftComplexSpec extends FlatSpec {

//  "Complex query #01" should "work as well" in {
//    val complex = Map(
//      "Stages" -> Map(
//        "$in" -> List(
//          Map(
//            "Stage" -> Map("Ordination" -> Map("$gt" -> 4))
//          )
//        )
//      )
//    )
//    def obj(ord: Int) = Map(
//      "Stages" -> List(
//        Map(
//          "Stage" -> Map("Ordination" -> ord)
//        )
//      )
//    )
//    assert(Sift(complex).testQuery(obj(5)))
//    assert(!Sift(complex).testQuery(obj(3)))
//  }
  /**
    * This example was taken by Redepag's deployment
    */
  "Complex query #02" should "work as well" in {
    val stages = Map(
      "Stages" -> Map(
        "$in" -> List(Map("Stage" -> Map("Ordination" -> Map("$gte" -> 3))))
      )
    )
    val orOnly = Map(
      "$or" -> List(
        Map(
          "OtherProperties" -> Map(
            "$in" -> List(Map(
              "FieldKey" -> "faturamento_total",
              "IntegerValue" -> Map("$exists" -> true)
            ))
          )
        )
      )
    )
    val full = Map(
      "After" -> Map(
        "Stages" -> Map(
          "$in" -> List(Map("Stage" -> Map("Ordination" -> Map("$gte" -> 3))))
        ),
        "$or" -> List(
          Map(
            "OtherProperties" -> Map(
              "$in" -> List(
                Map(
                  "FieldKey" -> "faturamento_total",
                  "IntegerValue" -> Map("$exists" -> false)
                )
              )
            )
          ),
          Map(
            "OtherProperties" -> Map(
              "$in" -> List(Map(
                "FieldKey" -> "faturamento_em_cartoes",
                "IntegerValue" -> Map("$exists" -> false)
              ))
            )
          )
        )
      )
    )

    def obj(ord: Int, faturamentoTotal: Option[Any], faturamentoEmCartoes: Option[Any]): Map[String, Any] = {
      val fatTotal: Any = faturamentoTotal.orNull
      val fatEmCartoes: Any = faturamentoEmCartoes.orNull
      Map(
        "After" -> Map(
          "Stages" -> List(
            Map(
              "Stage" -> Map("Ordination" -> ord)
            )
          ),
          "OtherProperties" -> List(
            Map(
              "FieldKey" -> "faturamento_total",
              "IntegerValue" -> fatTotal
            ),
            Map(
              "FieldKey" -> "faturamento_em_cartoes",
              "IntegerValue" -> fatEmCartoes
            )
          )
        )
      )
    }
    val allInputFull = obj(5, Some(42), Some(3))
    val oneNotFull = obj(5, None, Some(3))
    val oneNotFull2 = obj(5, Some(42), None)
    val noneFull = obj(5, None, None)
    val nullFull = obj(5, Some(null), Some(null))
    assert(!Sift(full).testQuery(allInputFull))
    assert(Sift(full).testQuery(oneNotFull))
    assert(Sift(full).testQuery(oneNotFull2))
    assert(Sift(full).testQuery(nullFull))
  }

  "Complex query #03" should "work as well" in {
    val full = Map(
      "After" -> Map(
        "Stages" -> Map(
          "$in" -> List(Map("Stage" -> Map("Ordination" -> Map("$gte" -> 4))))
        ),
        "OtherProperties" -> Map(
          "$in" -> List(Map(
            "FieldKey" -> "endereco_correto",
            "StringValue" -> "Não"
          ))
        ),
        "Attachments" -> Map(
          "$nin" -> List(Map(
            "FileName" -> Map(
              "$regex" -> "(?i)(?=(.*comprovante))(?=(.*endere[cç]o)).*"
            )
          ))
        )
      )
    )

    def obj(ord: Int, enderecoCorreto: Boolean, filename: String): Map[String, Any] = {
      val corretoStr = if(enderecoCorreto) "Sim" else "Não"

      Map(
        "After" -> Map(
          "Stages" -> List(
            Map(
              "Stage" -> Map("Ordination" -> ord)
            )
          ),
          "OtherProperties" -> List(
            Map(
              "FieldKey" -> "endereco_correto",
              "StringValue" -> corretoStr
            )
          ),
          "Attachments" -> List(Map(
            "FileName" -> filename
          ))
        )
      )
    }
    val shouldNotTriggerWhenEnderecoCorreto = obj(5, enderecoCorreto = true, "Comprovante de Endereço")
    val shouldNotTriggerWhenHasEnderecoComprovante = obj(5, enderecoCorreto = false, "Endereço Comprovante")
    val shouldTriggerWhenNoneItsRight = obj(5, enderecoCorreto = false, "Comprovante de residencia")
    assert(!Sift(full).testQuery(shouldNotTriggerWhenEnderecoCorreto))
    assert(!Sift(full).testQuery(shouldNotTriggerWhenHasEnderecoComprovante))
    assert(Sift(full).testQuery(shouldTriggerWhenNoneItsRight))
  }
}
