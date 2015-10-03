package de.codesourcery.simplevm.parser

sealed case class Identifier(name:String) 
{
  override def toString() : String = name
}