package de.codesourcery.simplevm.parser

import de.codesourcery.simplevm.parser.ast.AST
import de.codesourcery.simplevm.parser.ast.IASTNode
import de.codesourcery.simplevm.parser.ast.FunctionDefinition
import de.codesourcery.simplevm.parser.ast.Statement
import de.codesourcery.simplevm.parser.ast.IdentifierNode
import de.codesourcery.simplevm.parser.ast.ParameterList
import de.codesourcery.simplevm.parser.ast.Block
import de.codesourcery.simplevm.parser.ast.VariableDefinition
import de.codesourcery.simplevm.parser.ast.OperatorNode
import de.codesourcery.simplevm.parser.ast.IntLiteral
import de.codesourcery.simplevm.parser.ast.StringLiteral
import scala.collection.mutable.ArrayBuffer
import de.codesourcery.simplevm.parser.ast.FunctionArgumentsList
import de.codesourcery.simplevm.parser.ast.FunctionArgument

class Parser(lexer:ILexer) 
{
  private[this] var lastError : String = null
  
  def parse() : AST = 
  {
    val ast = new AST()
    while( ! lexer.eof ) 
    {
      parseStatement(ast.scope.get,true) match 
      {
        case Some(x) =>  ast.addChild( x )
        case None => throw new RuntimeException("Syntax error: "+lastError)
      }
      skipBlankLines()
    }
    ast
  }
  
  private[this] def fail(msg:String) : Option[IASTNode] =
  {
    lastError = msg +" @ offset "+lexer.peek 
    None
  }
  
  private[this] def parseStatement(scope:Scope,functionDefinitionAllowed:Boolean) : Option[IASTNode] = 
  {
    skipBlankLines()  
    val funcDef = if ( functionDefinitionAllowed ) parseFunctionDefinition(scope) else None
    if ( funcDef.isDefined ) {
      return funcDef
    }
    val varDef = parseVariableDefinition(scope)
    if ( varDef.isDefined ) {
      return varDef
    }
    parseExpression()
  }
  
  private[this] def parseFunctionDefinition(scope:Scope) : Option[IASTNode] = 
  {
    if ( consume( TokenType.FUNCTION_DEFINITION ) ) 
    {
       if ( lexer.peek(TokenType.IDENTIFIER) ) 
       {
         val identifier = Identifier( lexer.next().text )
         val func = new FunctionDefinition( identifier )
         scope.define( Symbol(identifier,SymbolType.FUNCTION_NAME , func ) )
         val signature = parseFunctionParameters( func.scope.get )
         if ( signature.isDefined ) 
         {
           func.addChild( signature.get )
           var additional = parseFunctionParameters( func.scope.get )
           while ( additional.isDefined ) {
              func.addChild( additional.get )
              additional = parseFunctionParameters( func.scope.get )
           }
           val block = parseBlock( func.scope.get )
           if ( block.isDefined ) 
           {
             func.addChild( block.get )
             return Some(func)
           }
           return fail("Function body expected")
         }
         return fail("Function parameter list expected")
       }
       return fail("function name expected")
    }
    fail("Expected a function definition")
  }
  
  private[this] def parseFunctionParameter(scope:Scope) : Option[IASTNode] = 
  {
    if ( lexer.peek(TokenType.IDENTIFIER) ) 
    {
      val id = Identifier( lexer.next().text )
      if ( consume(TokenType.COLON ) ) 
      {
         val typeName = parseTypeName()
         if ( typeName.isDefined )
         {
           val node = new FunctionArgument(id , typeName.get )
           scope.define( Symbol( id , SymbolType.VARIABLE , node ) )
           return Some( node )
         } 
         return fail("Expected a type name")
      } 
      return fail("Expected a ':'")
    }
    fail("Expected an method argument identifier")
  }
  
  private[this] def parseTypeName() : Option[TypeName] = 
  {
    val id = parseTypeNameWithQualifier()
    if ( id.isDefined  ) 
    {
      if ( consume(OperatorType.ARROW) ) 
      {
        if ( lexer.peek( TokenType.IDENTIFIER ) ) 
        {
            val id2 = parseTypeNameWithQualifier()
            if ( ! id2.isDefined )
            {
              return None
            }
            return Some( new TypeName( id.get +" => "+id2.get ) ) 
        }
        lastError = "Expected a return type identifier"
        return None
      }      
      return Some( new TypeName( id.get ) ) 
    }
    lastError = "Expected a valid type name"
    None
  }
  
  private[this] def parseTypeNameWithQualifier() : Option[String] = 
  {
    if ( lexer.peek(TokenType.IDENTIFIER) ) 
    {
      val id = lexer.next().text
      val isArray = parseArrayQualifier()
      if ( ! isArray.isDefined )
      {
        return None
      }
      return Some( if ( isArray.get) id+"[]" else id )
    }
    None
  }
  
  private[this] def parseArrayQualifier() : Option[Boolean] = 
  {
    if ( ! consume(TokenType.ANGLE_BRACKETS_OPEN ) ) {
      return Some(false)
    }
    if ( consume( TokenType.ANGLE_BRACKETS_CLOSE ) ) {
      return Some(true)
    }
    lastError = "Expected ']'"
    return None
  }
  
  private[this] def parseBlock(scope:Scope) : Option[IASTNode] = 
  {
    skipBlankLines()    
    if ( consume( TokenType.BRACES_OPEN ) ) 
    {
      val block = new Block
      var result : Option[IASTNode] = None
      do 
      {
        result = parseStatement(scope,false)
        result.map( child => block.addChild( child ) )
      } while ( result.isDefined )
      skipBlankLines()  
      if ( consume( TokenType.BRACES_CLOSE ) ) {
        return Some( block )
      }
    }
    fail("Expected a block")
  }
  
  private[this] def skipBlankLines() 
  {
    while( consume(TokenType.EOL) )  {
    }
  }
  
  private[this] def parseFunctionParameters(scope:Scope) : Option[IASTNode] = 
  {
    if ( consume( TokenType.PARENS_OPEN ) ) 
    {
      val params = new ParameterList
      var list = parseFunctionParameter(scope)
      while ( list.isDefined )
      {
          params.addChild( list.get )
          list = if ( consume( TokenType.COMMA ) ) parseFunctionParameter(scope) else None
      }
      if ( consume( TokenType.PARENS_CLOSE ) ) {
        return Some( params )
      }
      return fail("Unterminated function signature")
    }
    fail("Expected a function signature")
  }
  
  private[this] def consume(op : OperatorType) : Boolean = 
  {
    val tok = lexer.peek
    if ( tok.hasType( TokenType.OPERATOR ) && tok.text == op.symbol ) 
    {
      println("CONSUMED: "+lexer.next())
      return true
    } 
    false
  }  
  
  private[this] def consume(tt:TokenType) : Boolean = 
  {
    if ( lexer.peek( tt ) ) 
    {
      println("CONSUMED: "+lexer.next())
      return true
    } 
    return false
  }
  
  private[this] def parseVariableDefinition(scope:Scope) : Option[IASTNode] = 
  {
    if ( consume(TokenType.VARIABLE_DEFINITION ) ) 
    {
      val tok = lexer.peek
      if ( tok.hasType( TokenType.IDENTIFIER ) ) 
      {
        val identifier = Identifier( lexer.next().text )
        val result = new VariableDefinition(identifier)
        scope.define( Symbol(identifier,SymbolType.VARIABLE , result ) )
        if ( consume(OperatorType.ASSIGNMENT) ) 
        {
          val expr = parseExpression()
          if ( expr.isDefined ) 
          {
            result.addChild( expr.get )
            return Some(result)
          }
        }
        return fail("'=' expected")
      }
    }
    fail("Expected a variable definition")
  }
  
  private[this] def parseExpression() : Option[IASTNode] = 
  {
    val value = parseValue()
    if ( value.isDefined )
    {
      if ( lexer.peek( TokenType.OPERATOR ) ) 
      {
        val opTypes = OperatorType.getOperatorTypes( lexer.next().text )
        if ( opTypes.size != 1 ) {
          throw new RuntimeException("Ambiguous operator: "+opTypes);
        }
        val expr = new OperatorNode( opTypes.get(0) )
        expr.addChild( value.get )
        val value2 = parseExpression()
        if ( value2.isDefined ) 
        {
          expr.addChild( value2.get )
          return Some(expr)
        }
        return fail("operator lacks value")
      }
      return value
    }
    fail("Expected an expression")
  }
  
  private[this] def parseValue() : Option[IASTNode] = 
  {
     val tok = lexer.peek
     
     if ( consume( TokenType.DOUBLE_QUOTE) ) // parse string literal 
     {
       var string = ""
       while ( ! lexer.eof && ! lexer.peek(TokenType.DOUBLE_QUOTE ) && ! lexer.peek(TokenType.EOL ) ) 
       {
          string = string + lexer.next().text  
       }
       if ( consume( TokenType.DOUBLE_QUOTE ) ) {
         return Some( new StringLiteral( string ) )
       }
       return fail("Unterminated string literal") 
     }
     
     if ( tok.hasType( TokenType.NUMBER ) ) // parse number literal
     {
         return Some( new IntLiteral( lexer.next().text ) )  
     } 
     
     if ( tok.hasType( TokenType.IDENTIFIER ) ) // parse identifier or function call
     {
       val id = Identifier( lexer.next().text )
       val idNode = new IdentifierNode( id )
       val lists = parseFunctionArgumentLists()
       if ( lists.isDefined ) 
       {
         val functionCall = new OperatorNode(OperatorType.FUNCTION_CALL )
         functionCall.addChild( idNode )
         lists.get.foreach( functionCall.addChild )
         return Some(functionCall)
       }
       return Some( idNode )
     }
     fail("Expected a value")
  }
  
  private[this] def parseFunctionArgumentLists() : Option[ Seq[FunctionArgumentsList] ] = 
  {
    val result = new ArrayBuffer[FunctionArgumentsList]
    var tmp = parseFunctionArgumentList()
    if ( ! tmp.isDefined ) {
      lastError = "Expected function call argument list @ "+lexer.peek
      return None
    }
    result += tmp.get
    while ( tmp.isDefined ) {
      tmp = parseFunctionArgumentList()
      tmp.foreach( list => result += list )
    }
    return Some(result)
  }
  
  private[this] def isOpeningParens(tok : Token) : Boolean = tok.hasType(TokenType.PARENS_OPEN) || tok.hasType( TokenType.BRACES_OPEN )
  
  private[this] def parseFunctionArgumentList() : Option[ FunctionArgumentsList ] = 
  {
       val token = lexer.peek
       if ( isOpeningParens( token ) ) { // => function invocation
         lexer.next()
         var result = new FunctionArgumentsList
         var node = parseExpression()
         while ( node.isDefined ) 
         {
           result.addChild( node.get )
           node = if ( consume(TokenType.COMMA) ) parseExpression() else None
         }
         val closingType = if ( token.tokenType == TokenType.PARENS_OPEN ) TokenType.PARENS_CLOSE else TokenType.BRACES_CLOSE 
         if ( ! consume( closingType ) ) 
         {
           lastError = "Expected "+closingType+" @ "+lexer.peek
           return None
         }
         return Some(result)
       }
       return None
  }
}