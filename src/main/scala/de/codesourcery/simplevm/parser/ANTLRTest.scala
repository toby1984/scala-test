package de.codesourcery.simplevm.parser

import org.antlr.v4.runtime.ANTLRInputStream
import de.codesourcery.simplevm.grammar.minimallanguageLexer
import de.codesourcery.simplevm.grammar.minimallanguageParser
import org.antlr.v4.runtime.CommonTokenStream
import org.antlr.v4.runtime.tree.ParseTreeWalker
import de.codesourcery.simplevm.grammar.minimallanguageListener
import scala.collection.mutable.Stack
import de.codesourcery.simplevm.grammar.minimallanguageParser.InitContext
import de.codesourcery.simplevm.parser.ast.IASTNode
import de.codesourcery.simplevm.parser.ast.AST
import de.codesourcery.simplevm.grammar.minimallanguageParser.ValDefContext
import de.codesourcery.simplevm.grammar.minimallanguageBaseVisitor
import de.codesourcery.simplevm.parser.ast.VariableDefinition
import de.codesourcery.simplevm.grammar.minimallanguageParser.VarDefContext
import de.codesourcery.simplevm.grammar.minimallanguageParser.ValueContext
import de.codesourcery.simplevm.parser.ast.IntLiteral
import de.codesourcery.simplevm.parser.ast.StringLiteral
import de.codesourcery.simplevm.parser.ast.IdentifierNode
import de.codesourcery.simplevm.grammar.minimallanguageParser.GlobalScopeContext
import de.codesourcery.simplevm.grammar.minimallanguageParser.FunctionDeclarationContext
import de.codesourcery.simplevm.parser.ast.FunctionDeclaration
import de.codesourcery.simplevm.grammar.minimallanguageLexer
import de.codesourcery.simplevm.parser.Identifier
import de.codesourcery.simplevm.grammar.minimallanguageParser.ReturnTypeContext
import de.codesourcery.simplevm.parser.ast.ParameterList
import de.codesourcery.simplevm.grammar.minimallanguageParser.ParameterListContext
import de.codesourcery.simplevm.grammar.minimallanguageParser.ParameterDeclContext
import de.codesourcery.simplevm.parser.ast.FunctionArgumentsList
import de.codesourcery.simplevm.parser.ast.FunctionArgument
import de.codesourcery.simplevm.grammar.minimallanguageParser.SimpleTypeNameContext
import de.codesourcery.simplevm.grammar.minimallanguageParser.ArrayTypeNameContext

object ANTLRTest {
  
  def main(args:Array[String]) 
  {
    val code = """
               val a = 5 + 7 * 3
               """
    val input = new ANTLRInputStream( code )
    val lexer = new minimallanguageLexer( input )
    val tokenStream = new CommonTokenStream(lexer)
    val parser = new minimallanguageParser( tokenStream )
    
    val ast = parser.init()
    val visitor = new TreeVisitor
    ast.accept( visitor )
  }
}

class TreeVisitor extends minimallanguageBaseVisitor[java.util.List[IASTNode]] 
{
   val scopes = new Stack[Scope]()
   val ast : AST = new AST()
   
   var returnType : TypeName = null
   var paramType : TypeName = null
   
   implicit def seqToList(seq:Seq[IASTNode] ) : java.util.List[IASTNode] = 
   {
     val list = new java.util.ArrayList[IASTNode]()
     seq.foreach( x => list.add( x ) )
     list
   }
   
   implicit def nodeToList(node:IASTNode ) : java.util.List[IASTNode] = java.util.Collections.singletonList( node ) 
   
   override def visitGlobalScope(ctx:GlobalScopeContext) : java.util.List[IASTNode] = 
   {
      ast.addChildren( visitChildren( ctx ) ) 
      none
   }
   
   /*
functionDeclaration: 'def' IDENT parameterList returnType EOL;

functionDefinition: 'def' IDENT parameterList returnType block EOL;    
    */
   
   override def visitFunctionDeclaration(ctx:FunctionDeclarationContext) : java.util.List[IASTNode] = 
   {
     val funcName = Identifier( ctx.IDENT().getText )
     val node = new FunctionDeclaration(funcName, returnType,false)
     node.addChildren( visitChildren( ctx ) )
     node
   }

   override def visitParameterList(ctx:ParameterListContext) : java.util.List[IASTNode] = 
   {
     val node = new FunctionArgumentsList()
     node.addChildren( visitChildren(ctx) )
     node
   }
   
   override def visitParameterDecl(ctx:ParameterDeclContext) : java.util.List[IASTNode] = 
   {   
      val paramName = Identifier( ctx.IDENT().getText )
      visitChildren(ctx)
      new FunctionArgument(paramName,paramType)
   }
   
   override def visitSimpleTypeName(ctx:SimpleTypeNameContext) : java.util.List[IASTNode] = {
     paramType = TypeName( ctx.IDENT().getText() )
     none
   }
   
   override def visitArrayTypeName(ctx:ArrayTypeNameContext) : java.util.List[IASTNode] = {
     paramType = TypeName( ctx.IDENT().getText() )
     none
   }   
   
   override def visitReturnType(ctx:ReturnTypeContext) : java.util.List[IASTNode] = 
   {
     visitChildren(ctx)
     returnType = paramType
     none
   }
   
   override def visitValue(ctx:ValueContext) : java.util.List[IASTNode] = 
   {
     if ( ctx.IDENT() != null ) {
       new IdentifierNode( Identifier( ctx.IDENT().getText ) )
     } else if ( ctx.STRING() != null ) {
         new StringLiteral( ctx.NUMBER().getText )
     } else if ( ctx.NUMBER() != null ) {
       new IntLiteral( ctx.NUMBER().getText )
     } else {
       throw new RuntimeException("Unreachable code reached")
     }
   }
   
   override def visitValDef(ctx : ValDefContext ) : java.util.List[IASTNode] = 
   {
     val varName = Identifier( ctx.IDENT().getText )
     val result = new VariableDefinition( varName )
     result.addChildren( visitChildren(ctx) )
     result
   }
   
   override def visitVarDef(ctx : VarDefContext ) : java.util.List[IASTNode] = 
   {
     val varName = Identifier( ctx.IDENT().getText )
     val result = new VariableDefinition( varName )
     result.addChildren( visitChildren(ctx) )
     result
   }
   
   private[this] val none : java.util.List[IASTNode] = java.util.Collections.emptyList()
   
   private[this] def asList(node:IASTNode) : java.util.List[IASTNode] =       java.util.Collections.singletonList( node )
}