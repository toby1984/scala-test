package de.codesourcery.simplevm.parser

sealed case class Token(text:String,tokenType : TokenType,region:TextRegion) 
{
   def this( txt : Char , tokenType : TokenType,region:TextRegion ) = this( txt.toString , tokenType , region )
   
   def hasType(t : TokenType) : Boolean = tokenType == t
   
   def isEOF = tokenType == TokenType.EOF
   
   if ( tokenType == null ) {
     throw new IllegalArgumentException("token type must not be NULL")
   }
   if ( region == null ) {
     throw new IllegalArgumentException("region must not be NULL")
   }   
   if ( text == null ) {
     throw new IllegalArgumentException("text must not be NULL")
   }      
   if ( text.length != region.length ) {
     throw new IllegalArgumentException("Region length "+region.length+" does not match length of text >"+text+"<")
   }
}

object Token {
  
  def apply( txt : Char , tokenType : TokenType,region:TextRegion ) : Token = Token( txt.toString , tokenType , region )
}