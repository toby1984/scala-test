package de.codesourcery.simplevm.parser

import scala.collection.mutable.ArrayBuffer

class Lexer(private[this] val scanner:IScanner) extends ILexer 
{  
  private[this] val NUMBER = "^([0-9]+)$".r
  private[this] val IDENTIFIER = "^([a-zA-Z][_a-zA-Z0-9]*)$".r;
  
  private[this] val tokens = new ArrayBuffer[Token]
  private[this] val buffer = new java.lang.StringBuilder
  
  def eof : Boolean = peek.isEOF
  
  def next(): Token =  currentToken( true )

  def peek : Token = currentToken( false )   
  
  private[this] def currentToken(advance:Boolean) : Token =
  {
    if ( tokens.isEmpty ) {
       parse()
    }
    if ( advance ) tokens.remove(0) else tokens(0)
  }
  
  override def toString():String = peek.toString
  
  private[this] def parse() 
  {
    var offset = scanner.offset
    while ( ! scanner.eof & tokens.isEmpty ) 
    {
      val c = scanner.next()
      if ( isNoWhitespace( c ) ) 
      {
        getTerminalType( c ) match 
        {
          case Some(tt) => 
          {
            parseBuffer(offset)
            offset = scanner.offset
            tokens += Token( c , tt , TextRegion( offset-1 , 1 ) ) 
          }
          case None => buffer.append( c )
        }
      } else {
        parseBuffer(offset)
        offset = scanner.offset
      }
    }
    
    parseBuffer(offset)
    if ( scanner.eof ) {
      tokens += Token( "" , TokenType.EOF , TextRegion( scanner.offset , 0 ) )
    }
  }
  
  private[this] def isNoWhitespace(c:Char) : Boolean = c match 
  {
    case ' ' => false
    case '\t' => false
    case _ => true
  }
    
  private[this] def getTerminalType(c : Char ) : Option[TokenType] = c match 
  {
      case '\n' => Some( TokenType.EOL )
      case '('  => Some( TokenType.PARENS_OPEN )
      case ')'  => Some( TokenType.PARENS_CLOSE )
      case '{'  => Some( TokenType.BRACES_OPEN)
      case ':'  => Some( TokenType.COLON)
      case '}'  => Some( TokenType.BRACES_CLOSE)
      case ','  => Some( TokenType.COMMA )
      case x if OperatorType.isValidOperator( Character.toString(x) ) => Some( TokenType.OPERATOR )
      case _ => None
  }  

  private[this] def parseBuffer(bufferOffset:Int) 
  {
    val content = buffer.toString()
    val tokenType = content match 
    {
      case "def"                    => Some(TokenType.FUNCTION_DEFINITION)
      case "val"                    => Some(TokenType.VARIABLE_DEFINITION)
      case x if TypeName.isValidType( x ) => Some(TokenType.TYPENAME)
      case NUMBER(x)                => Some(TokenType.NUMBER)
      case IDENTIFIER(x)            => Some(TokenType.IDENTIFIER)
      case x if x.trim.length() > 0 => Some(TokenType.TEXT)
      case _                        => None
    } 
    tokenType.map( tt => tokens += Token( content , tt , TextRegion( bufferOffset , content.length ) ) )
    buffer.setLength(0)
  }
}