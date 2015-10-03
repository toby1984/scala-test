package de.codesourcery.simplevm.parser

class StringScanner(text:String) extends IScanner 
{
  private[this] var index = 0
  
  def eof: Boolean = index >= text.length

  def offset: Int = index

  def next(): Char = { 
    val c = text.charAt( index)
    index += 1
    c
  } 

  def peek: Char = text.charAt(index)
}