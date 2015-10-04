package de.codesourcery.simplevm.parser

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.Position

class StringScanner(protected val text:String) extends IScanner 
{   
  protected var index = 0
  
  override def eof: Boolean = index >= text.length

  override def offset: Int = index

  override def next(): Char = 
  { 
    val c = text.charAt( index )
    index += 1
    c
  } 
  
  override def peek: Char = text.charAt(index)
  
  if ( text == null ) {
    throw new IllegalArgumentException("text must not be NULL")
  }
}