package de.codesourcery.simplevm.parser

trait ILexer 
{  
  def eof : Boolean
  def peek : Token
  
  final def peek(tt:TokenType) : Boolean = peek.hasType( tt )
  
  def next() : Token
  
  def pushBack(tok:Token) : Unit
}