package de.codesourcery.simplevm.parser

sealed case class Token(text:String,tokenType : TokenType,position:Position) 
{
   def this( txt : Char , tokenType : TokenType,position:Position) = this( txt.toString , tokenType , position)
   
   def hasType(t : TokenType) : Boolean = tokenType == t
   
   def isEOF = tokenType == TokenType.EOF
   
   if ( tokenType == null ) {
     throw new IllegalArgumentException("token type must not be NULL")
   }
   if ( position == null ) {
     throw new IllegalArgumentException("position must not be NULL")
   }   
   if ( text == null ) {
     throw new IllegalArgumentException("text must not be NULL")
   }      
}

sealed case class Position ( line : Int, column : Int, offset:Int)

object Token 
{
  def apply( txt : Char , tokenType : TokenType,position:Position ) : Token = Token( txt.toString , tokenType , position )
}