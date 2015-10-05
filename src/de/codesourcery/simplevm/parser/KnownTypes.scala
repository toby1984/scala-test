package de.codesourcery.simplevm.parser

object KnownTypes 
{
  val STRING = TypeName("String")
  
  val INTEGRAL = TypeName("Int")
  
  val UNIT = TypeName("Unit")
  
  val NOTHING = TypeName("Nothing")
  
  val ANY = TypeName("Any")
  
  val UNKNOWN = TypeName("Unknown")  
  
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