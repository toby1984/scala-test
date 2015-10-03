package de.codesourcery.simplevm.parser

class StringScanner(text:String) extends IScanner 
{
  private[this] var index = 0
  
  override def eof: Boolean = index >= text.length

  override def offset: Int = index

  override def next(): Char = { 
    val c = text.charAt( index)
    index += 1
    c
  } 

  override def peek: Char = text.charAt(index)
  
  if ( text == null ) {
    throw new IllegalArgumentException("text must not be NULL")
  }
}