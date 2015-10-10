package de.codesourcery.simplevm.parser

trait IScanner 
{
   def eof : Boolean
   
   def peek : Char
   
   def next() : Char
   
   def position : Position
}