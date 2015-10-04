package de.codesourcery.simplevm.parser.ast

import de.codesourcery.simplevm.parser.Scope
import de.codesourcery.simplevm.parser.Identifier
import de.codesourcery.simplevm.parser.OperatorType
import de.codesourcery.simplevm.parser.TypeName
import de.codesourcery.simplevm.compiler.ICompilationParticipant
import de.codesourcery.simplevm.compiler.ICompilationContext
import de.codesourcery.simplevm.parser.KnownTypes
import de.codesourcery.simplevm.parser.SymbolType
import de.codesourcery.simplevm.parser.ValueSymbol
import de.codesourcery.simplevm.parser.LabelSymbol

class AST extends ASTNode
{
  override val scope = Some( Scope.createGlobalScope(this) )
  
  override def print(depth:Int) : String = " " * depth + children.map( _.print(depth+1) ).mkString("\n")
  
  override def evaluate() : TypedValue = TypedValue( None , KnownTypes.NOTHING )
  
  override def visit(ctx: ICompilationContext): Unit = 
  {
    ctx.pushScope( scope.get )
    super.visit( ctx )
    ctx.popScope()
  }
}

class Block extends ASTNode 
{
    override def print(depth:Int) : String = " " * depth + "{\n" + children.map( c => " " * (depth+1) + c.print(depth+1) ).mkString("\n") + "\n" + " " * depth + "}"
    
    override def evaluate() : TypedValue =  if (hasNoChildren) TypedValue(None,KnownTypes.UNIT) else lastChild.evaluate()
}

final class IntLiteral(text:String) extends ASTNode with ICompilationParticipant 
{
  override def print(depth: Int): String = text
  
  override def evaluate() : TypedValue = TypedValue(Some(text.toInt) , KnownTypes.INTEGRAL )

  override def visit(ctx: ICompilationContext): Unit = ctx.emitLoad( evaluate().value )
}

final class StringLiteral(val value:String) extends ASTNode 
{
  override def print(depth: Int): String = "\"" + value + "\""
  
  override def evaluate() : TypedValue = TypedValue(Some(value) , KnownTypes.STRING )
  
  override def visit(ctx: ICompilationContext): Unit = ctx.emitLoad( evaluate().value )
}

final class IdentifierNode(val name:Identifier) extends ASTNode 
{  
    override def print(depth:Int) : String = name.toString
    
    override def evaluate() : TypedValue = 
    {
      scope.get.getSymbol( name ) match 
      {
        case Some(x) => x.symbolType match 
        {
          case SymbolType.VARIABLE => x.asInstanceOf[ValueSymbol].node.evaluate()
          case _ => TypedValue( None , KnownTypes.UNKNOWN )
        }
        case None => TypedValue( None , KnownTypes.UNKNOWN )
      }
    }
    
  override def visit(ctx: ICompilationContext): Unit = 
  {
    val symbol = ctx.currentScope.getSymbol( name )
    ctx.emitLoad( symbol.get )
  }
}

final class FunctionArgument(val name:Identifier, val kind:TypeName) extends ASTNode with ICompilationParticipant 
{
  override def print(depth: Int): String = name.toString +" : " + kind.name
  
  override def visit(ctx: ICompilationContext): Unit = ctx.registerVariable( name , kind )
  
  override def evaluate() : TypedValue = TypedValue(None, kind )
}

final class Statement extends ASTNode
{
  override def print(depth:Int) : String = children.map( _.print(depth+1) ).mkString("\n")  
    
  override def evaluate() : TypedValue = firstChild.evaluate()
}

final class VariableDefinition(name:Identifier) extends ASTNode 
{
  override def print(depth: Int): String = " " * depth + "val " + name.toString +" = "+ children.map( _.print(0) ).mkString(" " )
  
  override def evaluate() : TypedValue = firstChild.evaluate()
  
  override def visit(ctx:ICompilationContext) : Unit = 
  {
    ctx.registerVariable( name , evaluate().kind )
    super.visit( ctx )
  }
}

final class OperatorNode(val operator:OperatorType) extends ASTNode 
{
  override def print(depth: Int): String = 
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
  
  override def visit(ctx:ICompilationContext) : Unit = 
  {

    operator match 
    {
      case OperatorType.PLUS => 
      {
        super.visit( ctx )        
        ctx.emitAdd()
      }
      case OperatorType.MINUS => 
      {
        super.visit( ctx )        
        ctx.emitSub()
      }
      case OperatorType.ASSIGNMENT => 
      {
         // child(0) is IdentifierNode
         child(1).visit(ctx)
         val identifier = child(0).asInstanceOf[IdentifierNode].name
         val value = child(1).evaluate()
         if ( value.value.isDefined ) 
         {
           val symbol = ctx.currentScope.getSymbol( identifier , SymbolType.VARIABLE ).get
           ctx.emitStore( symbol.asInstanceOf[ValueSymbol] )           
         } 
      }
      case OperatorType.FUNCTION_CALL => 
      {
        children.drop(1).foreach( _.visit( ctx ) ) // ignore IdentifierNode, we'll load the target address last
        
        val funcName = child(0).asInstanceOf[IdentifierNode].name
        println("Visiting function call to "+funcName)
        val target = scope.get.getSymbol( funcName , SymbolType.FUNCTION_NAME )
        println("Call resolves to "+target)
        target match 
        {
          case Some(symbol) =>  
          {
            ctx.emitLoad( symbol )
            ctx.emitJump()
          }
          case None => throw new RuntimeException("Call to unknown function "+funcName)
        }
      }
      case _ => throw new RuntimeException("visit(ICompilationContext) not implemented for operator "+operator)
    }
  }
  
  override def evaluate() : TypedValue = 
  {
    if ( operator == OperatorType.ASSIGNMENT ) 
    {
      // child(0) is IdentifierNode
      return child(1).evaluate()
    }
    operator.evaluate( children.toArray : _* )
  }
}

final class FunctionDefinition(val name:Identifier,var returnType : TypeName = null) extends ASTNode 
{
    override val scope = Some( new Scope( name.toString , this ) )
    
    override def setParent(p : IASTNode ) : Unit = 
    {
      super.setParent( p )
      scope.get.setParent( p.scope )
    }  
    
    override def print(depth:Int) : String = 
    {
      val argLists = children.filter( _.isInstanceOf[ParameterList] ).map( _.print(depth+1) ).mkString
      val body = children.drop(1).filter( ! _.isInstanceOf[ParameterList] ).map( _.print(depth) ).mkString
      return " " * depth + name.toString + argLists + " : "+returnType.name+"\n" + body
    }
    
    override def evaluate() : TypedValue = 
    {
      val value = uniqueChild( classOf[ Block ] ).get.evaluate()
      TypedValue( value.value , returnType )
    }
    
    override def visit(ctx: ICompilationContext): Unit = 
    {
      println("== Emitting "+name+" in scope "+ctx.currentScope.name+" === ")
      ctx.registerFunction( name )
      ctx.pushScope( scope.get )
      try {
        children.foreach( _.visit( ctx ) )
        ctx.emitReturn()
      } 
      finally 
      {
        ctx.popScope()
        ctx.registerFunction( name )        
      }
    }
    
    def parameterListCount : Int = children.count( _.isInstanceOf[ParameterList] )
    
    def parameterList(index:Int) : ParameterList = children.filter( _.isInstanceOf[ParameterList] ).drop(index).map( _.asInstanceOf[ParameterList]).head  
}

final class FunctionArgumentsList extends ASTNode 
{
  override def print(depth: Int): String = "( " + children.map( _.print(depth) ).mkString(" , ") + " )" 
}

final class ParameterList extends ASTNode 
{
    override def print(depth:Int) : String = "(" + children.map( _.print(0) ).mkString(",") + ")"
    
    def hasNoParameters : Boolean = hasNoChildren
}