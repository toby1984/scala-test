package de.codesourcery.simplevm.parser

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack
import scala.util.parsing.input.Position

class Lexer(protected val scanner:IScanner) extends ILexer 
{
  private[this] val NUMBER = "^([0-9]+)$".r
  private[this] val IDENTIFIER = "^([a-zA-Z][_a-zA-Z0-9]*)$".r;
  
  protected val tokens = new ArrayBuffer[Token]
  private[this] val buffer = new java.lang.StringBuilder
  
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
    while ( ! scanner.eof && tokens.isEmpty ) 
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
      case '['  => Some( TokenType.ANGLE_BRACKETS_OPEN )
      case ']'  => Some( TokenType.ANGLE_BRACKETS_CLOSE )      
      case '('  => Some( TokenType.PARENS_OPEN )
      case ')'  => Some( TokenType.PARENS_CLOSE )
      case '{'  => Some( TokenType.BRACES_OPEN)
      case '"'  => Some( TokenType.DOUBLE_QUOTE)
      case ':'  => Some( TokenType.COLON)
      case '}'  => Some( TokenType.BRACES_CLOSE)
      case ','  => Some( TokenType.COMMA )
      case _ => None
  }  

  private[this] def parseBuffer(bufferOffset:Int) = 
  {
    val content = buffer.toString
    var head = 0
    var startIndex = 0
    while ( startIndex < content.length ) 
    {
       var offset = 1
       if ( (startIndex+offset) <= content.length && OperatorType.isValidOperator( content.substring( startIndex , startIndex + offset ) ) )
       {
          while ( startIndex+offset+1 <= content.length && OperatorType.isValidOperator( content.substring( startIndex , startIndex + offset+1 ) ) ) { 
            offset += 1
          }
       }
       if ( (startIndex+offset) <= content.length && OperatorType.isValidOperator( content.substring( startIndex , startIndex + offset ) ) ) 
       {
         if ( head != startIndex ) 
         {
           val str = content.substring( head , startIndex )
           val tokenType = getTokenType( str )     
           tokenType.map( tt => tokens += Token( str , tt , TextRegion( bufferOffset+head , startIndex - head ) ) )
         }
         tokens += Token( content.substring( startIndex , startIndex + offset ) , TokenType.OPERATOR , TextRegion( bufferOffset + startIndex + offset , offset ) )
         startIndex += offset
         head = startIndex
       } 
       else 
       {
         startIndex += 1
       }
    }
    if ( head != startIndex ) 
    {
     val str = content.substring( head , startIndex )
     val tokenType = getTokenType( str )     
     tokenType.map( tt => tokens += Token( str , tt , TextRegion( bufferOffset+head , startIndex - head ) ) )
    }
    buffer.setLength(0)
  }
  
  private[this] def getTokenType(s : String) : Option[TokenType] =  s match 
  {
    case "def"                    => Some(TokenType.FUNCTION_DEFINITION)
    case "val"                    => Some(TokenType.FINAL_VAR)
    case "var"                    => Some(TokenType.MUTABLE_VAR)
    case NUMBER(x)                => Some(TokenType.NUMBER)
    case IDENTIFIER(x)            => Some(TokenType.IDENTIFIER)
    case x if x.trim.length() > 0 => Some(TokenType.TEXT)
    case _                        => None
  }
}