package de.codesourcery.simplevm.parser

sealed case class Token(text:String,tokenType : TokenType,region:TextRegion) 
{
   def this( txt : Char , tokenType : TokenType,region:TextRegion ) = this( txt.toString , tokenType , region )
   
   def hasType(t : TokenType) : Boolean = tokenType == t
   
   def isEOF = tokenType == TokenType.EOF
}

object Token {
  
  def apply( txt : Char , tokenType : TokenType,region:TextRegion ) : Token = Token( txt.toString , tokenType , region )
}