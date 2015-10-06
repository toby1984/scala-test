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
import scala.collection.mutable.ListBuffer
import de.codesourcery.simplevm.parser.ast.FunctionDeclaration
import de.codesourcery.simplevm.Misc

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
    val stmt = new Statement()
    def addChildAndReturn(n:Option[IASTNode]) : Some[IASTNode] = {
      stmt.addChild( n.get )
      Some( stmt )
    }
    
    val funcDef = if ( functionDefinitionAllowed ) parseFunctionDefOrDecl(scope) else None
    if ( funcDef.isDefined ) 
    {
      return addChildAndReturn( funcDef )
    }
    
    val varDef = parseVariableDefinition(scope)
    if ( varDef.isDefined ) {
      return addChildAndReturn( varDef )
    }
    parseExpression() match {
      case node @ Some(_) => addChildAndReturn( node )
      case _ => None
    }
  }
  
  private[this] def parseFunctionDefOrDecl(parentScope:Scope) : Option[IASTNode] = 
  {
    val isExternal = consume(TokenType.EXTERNAL)
    if ( consume( TokenType.FUNCTION_DEFINITION ) ) 
    {
       if ( lexer.peek(TokenType.IDENTIFIER) ) 
       {
         val name = Identifier( lexer.next().text )
         
         val newScope = new Scope( name.name , Some(parentScope ) )
         
         val signature = parseFunctionParameters( Some( newScope ) )
         if ( ! signature.isDefined ) 
         {
           return fail("Function parameter list expected")           
         }
         
         val children = ListBuffer[IASTNode]()
         children += signature.get
         children ++ Misc.repeatUntilNone( parseFunctionParameters( Some( newScope ) ) )

         val returnType = parseReturnType()
         if ( ! returnType.isDefined ) {
           return fail("Function definition lacks return type")
         }
         
         val block = parseBlock( newScope )
         if ( block.isDefined ) // found body => function definition 
         {
           if ( isExternal ) {
             return fail("external function cannot have body")
           }
           val func = new FunctionDefinition(name,newScope,returnType.get)           
           func.addChildren( children )
           func.addChild( block.get )
           parentScope.defineLabel( name , SymbolType.FUNCTION_NAME , func )
           return Some( func )
         } 
         // no body => function declaration
         val func = new FunctionDeclaration(name,returnType.get,isExternal)           
         func.addChildren( children )
         parentScope.defineLabel( name , SymbolType.FUNCTION_NAME , func )           
         return Some( func )
       }
       return fail("function name expected")
    }
    fail("Expected a function definition")
  }
  
  private[this] def parseReturnType() : Option[TypeName] = if ( consume( TokenType.COLON ) ) parseTypeName() else None
  
  private[this] def parseFunctionParameter(scope:Option[Scope]) : Option[IASTNode] = 
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
           if ( scope.isDefined ) {
             scope.get.defineFinalValue( id , SymbolType.VARIABLE , node )
           }
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
      block.addChildren( Misc.repeatUntilNone( parseStatement(scope,false) ) )
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
  
  private[this] def parseFunctionParameters(scope:Option[Scope]) : Option[IASTNode] = 
  {
    if ( consume( TokenType.PARENS_OPEN ) ) 
    {
      val params = new ParameterList
      val list = parseOptionalCommaSeparatedValuesUsing( parseFunctionParameter(scope) )
      if ( ! list.isDefined ) {
        return None
      }
      params.addChildren( list.get )
      
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
      lexer.next()
      return true
    } 
    false
  }  
  
  private[this] def consume(tt:TokenType) : Boolean = 
  {
    if ( lexer.peek( tt ) ) 
    {
      lexer.next
      return true
    } 
    return false
  }
  
  private[this] def parseVariableDefinition(scope:Scope) : Option[IASTNode] = 
  {
    val isImmutable = consume(TokenType.FINAL_VAR )
    val isMutable = ! isImmutable && consume(TokenType.MUTABLE_VAR)
    if ( isImmutable || isMutable ) 
    {
      val tok = lexer.peek
      if ( tok.hasType( TokenType.IDENTIFIER ) ) 
      {
        val identifier = Identifier( lexer.next().text )
        val result = new VariableDefinition(identifier)
        if ( isImmutable ) {
          scope.defineFinalValue( identifier,SymbolType.VARIABLE , result )
        } else {
          scope.defineMutableValue( identifier,SymbolType.VARIABLE , result )
        }
        if ( consume(OperatorType.ASSIGNMENT) ) 
        {
          val expr = parseExpression()
          if ( expr.isDefined ) 
          {
            val assignment = new OperatorNode(OperatorType.ASSIGNMENT)
            assignment.addChild( new IdentifierNode( identifier ) )
            assignment.addChild( expr.get )
            result.addChild( assignment )
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
  
  private[this] def parseFunctionArgumentLists() : Option[ Seq[IASTNode] ] = 
  {
    val list = parseOptionalCommaSeparatedValuesUsing( parseFunctionArgumentList() )
    if ( ! list.isDefined ) {
      return None
    }
    if ( list.get.isEmpty ) {
      lastError = "Expected function call argument list @ "+lexer.peek
      return None
    }
    list
  }
  
  private[this] def isOpeningParens(tok : Token) : Boolean = tok.hasType(TokenType.PARENS_OPEN) || tok.hasType( TokenType.BRACES_OPEN )
  
  private[this] def parseFunctionArgumentList() : Option[ IASTNode ] = 
  {
       val token = lexer.peek
       if ( isOpeningParens( token ) ) { // => function invocation
         lexer.next()
         val result = new FunctionArgumentsList
         val args = parseOptionalCommaSeparatedValuesUsing( parseExpression )
         if ( ! args.isDefined ) {
           return None
         }
         result.addChildren( args.get )

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
  
  private[this] def parseOptionalCommaSeparatedValuesUsing( parse : => Option[IASTNode] ) : Option[ Seq[IASTNode] ] = 
  {
      val result = ListBuffer[IASTNode]()
      var tmp = parse
      while ( tmp.isDefined ) 
      {
         result += tmp.get
         if ( consume(TokenType.COMMA) ) 
         {
           tmp = parse
           if ( ! tmp.isDefined ) 
           {
             lastError = "Expected another value after comma"
             return None
           }
         } else {
           tmp = None
         }
      }
      Some( result )
  }
}