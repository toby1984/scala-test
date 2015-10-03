package de.codesourcery.simplevm.parser

object LexerTest {
  
  def main(args:Array[String]) 
  {
    val scanner = new StringScanner("1 => 2")
    val lexer = new Lexer( scanner )
    while ( ! lexer.eof ) 
    {
      val token = lexer.next()
      println( "Got "+token+" [ EOF: "+token.isEOF+" ]" )
    }
  }
}