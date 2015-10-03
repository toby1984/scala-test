package de.codesourcery.simplevm.parser

import scala.collection.mutable.HashMap

final class Scope(val name:String) 
{
    private[this] val symbols = new HashMap[Identifier,Symbol]
    private[this] var parent:Option[Scope] = None
    
    def define( s: Symbol ) 
    {
      if ( symbols.contains( s.name ) ) {
        throw new RuntimeException("Duplicate symbol "+s.name)
      }
      symbols += s.name -> s
    }
    
    def getSymbols : Seq[Symbol] = symbols.values.toSeq
    
    def getSymbol(identifier:Identifier) : Option[Symbol] = 
    {
      symbols.get( identifier ) match 
      {
        case None => parent.flatMap( _.getSymbol( identifier ) )
        case x => x  
      }
    }
    
    def getAllSymbols : Map[Identifier,Symbol] = 
    {
        val result = if ( parent.isDefined ) parent.get.getAllSymbols else Map()
        return result ++ symbols
    }
    
    def setParent(s : Option[Scope] ) = this.parent = s
    
    def identifier : String = if ( parent.isDefined ) parent.get.identifier + "." + name else name 
    
    override def toString() : String = identifier+"{"+symbols+"}"
    
    if ( name == null || name.trim.length == 0 ) {
      throw new IllegalArgumentException("name must not be NULL/blank")
     }
}