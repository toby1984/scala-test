package de.codesourcery.simplevm.parser

trait IScanner 
{
   def offset : Int
   def eof : Boolean
   def peek : Char
   def next() : Char
}