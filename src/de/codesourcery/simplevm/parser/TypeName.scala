package de.codesourcery.simplevm.parser

sealed case class TypeName(name:String) {
  
  def isArrayType : Boolean = name.endsWith("[]")
  
  def isFunctionType : Boolean = name.contains("=>")
  
  def sourceType : TypeName = if ( isFunctionType ) TypeName( name.split("=>")(0).trim ) else this
  
  def targetType : TypeName = if ( isFunctionType ) TypeName( name.split("=>")(1).trim ) else this
  
  def isKnownType : Boolean = TypeName.isKnownType( sourceType ) && TypeName.isKnownType( targetType ) 
  
  override def toString() : String = ">"+name + "<"
  
  if ( name == null || name.trim.length == 0 ) {
    throw new IllegalArgumentException("Name must not be NULL/blank")
  }
}

object TypeName 
{
  def isKnownType( toCheck : TypeName ) : Boolean = 
  {
    if ( toCheck.isFunctionType ) {
      return isKnownType( toCheck.sourceType ) && isKnownType( toCheck.targetType )    
    }
    val rawTypeName = if ( toCheck.isArrayType ) toCheck.name.substring( 0 , toCheck.name.length-2 ) else toCheck.name
    rawTypeName match 
    {
      case "int8" => true
      case "string" => true
      case x => 
      {
        System.err.println("Invalid type name: "+toCheck)    
        false
      }
    }
  }  
}