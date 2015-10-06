package de.codesourcery.simplevm.parser

import scala.collection.mutable.HashMap
import de.codesourcery.simplevm.parser.ast.IASTNode
import de.codesourcery.simplevm.parser.ast.AST
import scala.collection.mutable.ListBuffer

final class Scope(val name:String,val parent :Option[Scope]) 
{
    private[this] val symbols = new HashMap[Identifier,Symbol]
    
    def this(name:String) = this(name,None)
    
    def defineMutableValue( name:Identifier,symbolType:SymbolType, node:IASTNode) 
    {
      defineValue(name,symbolType,false,node)
    }
    
    def defineFinalValue( name:Identifier,symbolType:SymbolType, node:IASTNode) 
    {
      defineValue(name,symbolType,true,node)
    }    
    
    private[this] def defineValue( name:Identifier,symbolType:SymbolType, isImmutable: Boolean, node:IASTNode) 
    {
      val s = new ValueSymbol(name,symbolType,node,isImmutable)
      println("Defining "+(if ( isImmutable ) "final" else "mutable" )+" variable "+name+" in scope "+this)
      if ( symbols.contains( s.name ) ) {
        throw new RuntimeException("Duplicate symbol "+s.name)
      }
      symbols += s.name -> s
    }
    
    def defineLabel( name:Identifier,symbolType:SymbolType, node:IASTNode) : LabelSymbol =
    {
      val s = new LabelSymbol(name,symbolType,node)
      println("Defining label "+name+" in scope "+this)
      if ( symbols.contains( s.name ) ) {
        throw new RuntimeException("Duplicate symbol "+s.name)
      }
      symbols += s.name -> s
      s
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
    
    def getSymbol(identifier:Identifier,tt:SymbolType) : Option[Symbol] = 
    {
      symbols.get( identifier ) match 
      {
        case None => parent.flatMap( _.getSymbol( identifier , tt ) )
        case y @ Some(x) if x.symbolType == tt => y  
      }
    }    
    
    def fullyQualifiedName : String = 
    {
      var current : Option[Scope] = Some(this)
      val list = ListBuffer[Scope]()
      while ( current.isDefined ) 
      {
        list += current.get
        current = current.get.parent
      }
      list.reverse.map( s => s.name ).mkString(".")
    }    
    
    def fullyQualifiedName(symbol:Symbol) : String = 
    {
      val prefix = getOwningScope(symbol).get.fullyQualifiedName
      prefix+"."+symbol.name
    }
    
    def isInThisScope(symbol:Symbol) : Boolean = 
    {
      getOwningScope(symbol) match 
      {
        case None => false
        case Some(scope) => scope == this
      }
    }
    
    def getOwningScope(symbol:Symbol) : Option[Scope] = 
    {
      val existing = symbols.get(symbol.name)
      existing match 
      {
        case Some(_) => Some(this)
        case None => if ( hasParent ) parent.get.getOwningScope(symbol ) else None
      }
    }
    
    def hasParent : Boolean = parent.isDefined
    
    def getAllSymbols : Map[Identifier,Symbol] = 
    {
        val result = if ( parent.isDefined ) parent.get.getAllSymbols else Map()
        return result ++ symbols
    }
    
    def identifier : String = if ( parent.isDefined ) parent.get.identifier + "." + name else name 
    
    def isGlobalScope : Boolean = name == Scope.GLOBAL_SCOPE_NAME
      
    override def toString() : String = 
    {
      val map = symbols.map( pair => pair match { case (k,v) => k.toString + "=" + v.toString } )
      fullyQualifiedName+"{"+map.mkString(" | ")+"}"
    }
    
    if ( name == null || name.trim.length == 0 ) {
      throw new IllegalArgumentException("name must not be NULL/blank")
    }
}

object Scope {
  
  val GLOBAL_SCOPE_NAME = "<global>"
  
  def createGlobalScope() : Scope = new Scope( GLOBAL_SCOPE_NAME )
}