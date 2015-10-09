package de.codesourcery.simplevm.parser

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

class Lexer(protected val scanner:IScanner) extends ILexer 
{
  private[this] val NUMBER = "^([0-9]+)$".r
  private[this] val IDENTIFIER = "^([a-zA-Z][_a-zA-Z0-9]*)$".r;
  
  protected val tokens = new ArrayBuffer[Token]
  
  override def eof : Boolean = peek.isEOF
  
  override def next(): Token =  currentToken( true )

  override def peek : Token = currentToken( false )   
  
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
    val buffer = new java.lang.StringBuilder
    while( ! scanner.eof && isWhitespace( scanner.peek ) ) {
      scanner.next()
    }
    while ( ! scanner.eof && tokens.isEmpty ) 
    {
      val c = scanner.next()
      if ( isNoWhitespace( c ) ) 
      {        
        if ( OperatorType.isValidOperator( c ) ) 
        {
          parseBuffer( buffer , offset )
          offset = scanner.offset-1
          buffer.append( c )
          while ( ! scanner.eof && OperatorType.isValidOperator( buffer.toString()+scanner.peek ) ) {
            buffer.append( scanner.next() )
          }
          val tok = buffer.toString
          tokens += Token( tok ,TokenType.OPERATOR, TextRegion(offset,tok.length() ) )
          return
        } 
        val tt = getTerminalType(c)
        if ( tt != null ) 
        {
            parseBuffer(buffer , offset)
            tokens += Token( c , tt , TextRegion( scanner.offset-1 , 1 ) )
            return
        }
        buffer.append( c )
      } 
      else 
      { // whitespace found
        parseBuffer(buffer,offset)
        return
      }
    }
    
    parseBuffer(buffer , offset)
    if ( scanner.eof ) {
      tokens += Token( "" , TokenType.EOF , TextRegion( scanner.offset , 0 ) )
    }
  }
  
  private[this] def isNoWhitespace(c:Char) : Boolean = ! isWhitespace(c)
  
  private[this] def isWhitespace(c:Char) : Boolean = c match 
  {
    case ' ' => true
    case '\t' => true
    case _ => false
  }  
    
  private[this] def getTerminalType(c : Char ) : TokenType = c match // intentionally not using Option[TokenType] here because of performance 
  {
      case '\n' => TokenType.EOL
      case '['  => TokenType.ANGLE_BRACKETS_OPEN
      case ']'  => TokenType.ANGLE_BRACKETS_CLOSE
      case '('  => TokenType.PARENS_OPEN
      case ')'  => TokenType.PARENS_CLOSE
      case '{'  => TokenType.BRACES_OPEN
      case '"'  => TokenType.DOUBLE_QUOTE
      case ':'  => TokenType.COLON
      case '}'  => TokenType.BRACES_CLOSE
      case ','  => TokenType.COMMA
      case _ => null 
  }  

  private[this] def parseBuffer(buffer:java.lang.StringBuilder,bufferOffset:Int) = 
  {
    val str = buffer.toString()
    if ( str.length() > 0 ) 
    {
      val tokenType = str match 
      {
       case "def"                    => TokenType.FUNCTION_DEFINITION
       case "val"                    => TokenType.FINAL_VAR
       case "var"                    => TokenType.MUTABLE_VAR
       case "external"               => TokenType.EXTERNAL
       case NUMBER(x)                => TokenType.NUMBER
       case IDENTIFIER(x)            => TokenType.IDENTIFIER
       case x => TokenType.TEXT
      }
      tokens += Token( str , tokenType , TextRegion( bufferOffset , str.length() ) )
      buffer.setLength(0)
    }
  }
}