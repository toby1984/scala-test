package de.codesourcery.simplevm.parser

sealed case class Identifier(name:String) 
{
  override def toString() : String = name
  
  if ( name == null || name.trim.length == 0 ) {
    throw new IllegalArgumentException("Identifier must not be NULL/blank")
  }
}