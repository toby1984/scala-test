package de.codesourcery.simplevm.parser

sealed case class TypeName(name:String) {
  
  def isArrayType : Boolean = name.endsWith("[]")
  
  def isFunctionType : Boolean = name.contains("=>")
  
  def sourceType : TypeName = if ( isFunctionType ) TypeName( name.split("=>")(0).trim ) else this
  
  def targetType : TypeName = if ( isFunctionType ) TypeName( name.split("=>")(1).trim ) else this
  
  def isKnownType : Boolean = KnownTypes.isKnownType( sourceType ) && KnownTypes.isKnownType( targetType ) 
  
  override def toString() : String = ">"+name + "<"
  
  if ( name == null || name.trim.length == 0 ) {
    throw new IllegalArgumentException("Name must not be NULL/blank")
  }
}
