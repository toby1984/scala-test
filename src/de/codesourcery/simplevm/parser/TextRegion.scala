package de.codesourcery.simplevm.parser

sealed case class TextRegion(offset:Int,length:Int) {
  
  def merge(other:TextRegion) : TextRegion = TextRegion( Math.min( start , other.start ) , Math.max( end , other.end ) )
  
  def start : Int = offset
  def end : Int = offset+length
  
  if ( offset < 0 ) {
    throw new RuntimeException("Offset must not be negative")
  }
  
  if ( length < 0 ) {
    throw new RuntimeException("Length must not be negative")
  }
}