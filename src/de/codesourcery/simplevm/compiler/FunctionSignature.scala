package de.codesourcery.simplevm.compiler

import de.codesourcery.simplevm.parser.Identifier
import de.codesourcery.simplevm.parser.TypeName
import java.util.regex.Pattern
import scala.collection.mutable.ListBuffer

sealed case class FunctionSignature(val name:Identifier,val params:Seq[ Seq[TypeName] ],val returnType:TypeName) 
{  
  def this(name:Identifier,returnType:TypeName) 
  {
    this(name,List[Seq[TypeName]](),returnType)
  }
  
  def getAsString() : String = 
  {
    val arguments = if ( params.isEmpty ) {
      "()"
    } else {
      params.map( list => "("+list.mkString(",")+")" ).mkString
    }
    name+"|"+arguments+"|"+returnType.name
  }
  
  override def toString : String = getAsString()
}

object FunctionSignature {
  
  def fromString(s:String) : FunctionSignature = 
  {
    val parts = s.split("\\|")
    val name = Identifier(parts(0))
    val pattern = Pattern.compile("\\((.*?)\\)")
    val matcher = pattern.matcher( parts(1) )
    val params = new ListBuffer[Seq[TypeName]]()
    while ( matcher.find() ) {
      val list = matcher.group(1)
      val elements = list.split(",").map( TypeName ).toSeq
      params += elements
    }
    val returnType = TypeName(parts(2))
    new FunctionSignature(name,params,returnType)
  }
  
  def main(args:Array[String]) : Unit = {
    
    val test = new FunctionSignature(Identifier("print") , List( List( TypeName("Int") ) , List( TypeName("String") ) ) , TypeName("Unit") )
    println( test.getAsString() )
    val test2 = fromString( test.getAsString() )
    println( test2.getAsString() )
  }
}
    