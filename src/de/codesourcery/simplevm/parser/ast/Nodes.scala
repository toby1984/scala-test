package de.codesourcery.simplevm.parser.ast

import de.codesourcery.simplevm.parser.Scope
import de.codesourcery.simplevm.parser.Identifier
import de.codesourcery.simplevm.parser.OperatorType
import de.codesourcery.simplevm.parser.TypeName

class AST extends ASTNode
{
  override val scope = Some( new Scope("_gobal_") )
  
  override def print(depth:Int) : String = " " * depth + children.map( _.print(depth+1) ).mkString("\n")
}

class Block extends ASTNode 
{
    override def print(depth:Int) : String = " " * depth + "{\n" + children.map( c => " " * (depth+1) + c.print(depth+1) ).mkString("\n") + "\n" + " " * depth + "}"
}

final class IntLiteral(value:String) extends ASTNode 
{
  def print(depth: Int): String = value
}

final class StringLiteral(text:String) extends ASTNode 
{
  def print(depth: Int): String = "\"" + text + "\""
}

final class IdentifierNode(val name:Identifier) extends ASTNode {
  
    override def print(depth:Int) : String = name.toString
}

final class FunctionArgument(val name:Identifier, kind:TypeName) extends ASTNode 
{
  def print(depth: Int): String = name.toString +" : " + kind.symbol
}

final class Statement extends ASTNode
{
    override def print(depth:Int) : String = children.map( _.print(depth+1) ).mkString("\n")  
}

final class VariableDefinition(name:Identifier) extends ASTNode 
{
  def print(depth: Int): String = " " * depth + "val " + name.toString +" = "+ children.map( _.print(0) ).mkString(" " )
}

final class OperatorNode(operator:OperatorType) extends ASTNode 
{
  def print(depth: Int): String = 
  {
    if ( operator == OperatorType.FUNCTION_CALL ) {
            child(0).print(0)+ children.drop(1).map( _.print(0) ).mkString(" , ")
    } 
    else if ( operator.isInfixOperator() ) 
    {
      child(0).print(0)+" "+operator.symbol+" "+children.drop(1).map( _.print(0) ).mkString(" ")
    } else {
      children.map( _.print(0) ).mkString(" ")
    }
  }
}

final class FunctionDefinition(val name:Identifier) extends ASTNode 
{
    override val scope = Some( new Scope( "<func>"+name.toString ) )
    
    override def setParent(p : IASTNode ) : Unit = 
    {
      super.setParent( p )
      scope.get.setParent( p.scope )
    }  
    
    override def print(depth:Int) : String = 
    {
      val argLists = children.filter( _.isInstanceOf[ParameterList] ).map( _.print(depth+1) ).mkString
      val body = children.drop(1).filter( ! _.isInstanceOf[ParameterList] ).map( _.print(depth) ).mkString
      return " " * depth + name.toString + argLists + "\n" + body
    }
}

final class FunctionArgumentsList extends ASTNode 
{
  def print(depth: Int): String = "( " + children.map( _.print(depth) ).mkString(" , ") + " )" 
}

final class ParameterList extends ASTNode 
{
    override def print(depth:Int) : String = "(" + children.map( _.print(0) ).mkString(",") + ")"
}