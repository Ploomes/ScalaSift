package runner

import javax.script.ScriptEngineManager

import scala.tools.nsc.interpreter.Scripted

object Runner {

  def evalExpression(obj: Map[String, Any])(expr: String): Any = {
    //  Create the script engine
    val javaxEngine = new ScriptEngineManager().getEngineByName("scala")
    val scalaEngine = javaxEngine.asInstanceOf[Scripted]
    scalaEngine.intp.bind("obj", "Map[String, Any]", obj)
    scalaEngine.eval(expr)
  }
}