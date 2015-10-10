package de.codesourcery.simplevm.parser

import scala.collection.mutable.ListBuffer

class StringScanner(protected val text:String) extends IScanner 
{   
  protected var index = 0
  
  protected var line = 1
  
  protected var column = 1
  
  override def eof: Boolean = index >= text.length

  override def next(): Char = 
  { 
    val c = text.charAt( index )
    if ( c == '\n' ) {
      line += 1
      column = 1
    } else {
      column += 1
    }
    index += 1
    c
  }
  
  override def position : Position = Position(line,column,index)
  
  override def peek: Char = text.charAt(index)
  
  if ( text == null ) {
    throw new IllegalArgumentException("text must not be NULL")
  }
}