package de.codesourcery.simplevm.parser

object KnownTypes 
{
  val STRING = TypeName("string")
  
  val INTEGRAL = TypeName("int")
  
  val UNIT = TypeName("unit")
  
  val NOTHING = TypeName("nothing")
  
  val ANY = TypeName("any")
  
  val UNKNOWN = TypeName("unknown")  
  
  val ALL_VALID = List[TypeName]( STRING , INTEGRAL , UNIT , NOTHING , ANY )
  
  def isKnownType( toCheck : TypeName ) : Boolean = 
  {
    if ( toCheck.isFunctionType ) {
      return isKnownType( toCheck.sourceType ) && isKnownType( toCheck.targetType )    
    }
    val rawTypeName = if ( toCheck.isArrayType ) toCheck.name.substring( 0 , toCheck.name.length-2 ) else toCheck.name
    ALL_VALID.exists( t => t.name == rawTypeName )  
  }   
}