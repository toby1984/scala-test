package de.codesourcery.simplevm.parser

object LexerTest {
  
  def main(args:Array[String]){
    val scanner = new StringScanner("""    
        def(a,b) { c }""")
    val lexer = new Lexer( scanner )
    while ( ! lexer.eof ) 
    {
      val token = lexer.next()
      println( "Got "+token+" [ EOF: "+token.isEOF+" ]" )
    }
  }
}