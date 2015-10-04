package de.codesourcery.simplevm.parser

import scala.util.parsing.input.Position

trait ILexer 
{  
  def eof : Boolean
  def peek : Token
  
  final def peek(tt:TokenType) : Boolean = peek.hasType( tt )
  
  def next() : Token
}