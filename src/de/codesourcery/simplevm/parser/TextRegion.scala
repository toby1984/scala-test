package de.codesourcery.simplevm.parser

import scala.util.parsing.input.Position

sealed case class TextRegion(offset:Int,length:Int) {
  
  def start : Int = offset
  
  def end : Int = offset+length
  
  if ( offset < 0 ) {
    throw new RuntimeException("Offset must not be negative")
  }
  
  if ( length < 0 ) {
    throw new RuntimeException("Length must not be negative")
  }
}