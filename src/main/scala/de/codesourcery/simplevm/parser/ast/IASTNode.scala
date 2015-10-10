package de.codesourcery.simplevm.parser.ast

import de.codesourcery.simplevm.parser.Scope
import de.codesourcery.simplevm.compiler.ICompilationParticipant

trait IASTNode extends Iterable[IASTNode] with ICompilationParticipant
{
  def scope : Option[Scope]
  
  def childCount : Int
  
  final def addChildren(c : Iterable[IASTNode] ) : Unit =  c.foreach( addChild )
  
  final def addChildren(c : java.util.Collection[IASTNode] ) : Unit =  
  {
    val it = c.iterator()
    while ( it.hasNext() ) {
      addChild( it.next() )
    }
  }
  
  def addChild(child:IASTNode) : IASTNode
  
  def child(idx:Int) : IASTNode
  
  final def uniqueChild[T <: IASTNode ]( c : Class[T] ) : Option[IASTNode] = 
  {
     val tmp = children.filter( child => child.getClass == c )
     if ( tmp.isEmpty ) {
       None
     } else if ( tmp.size == 1 ) {
       Some(tmp(0))
     } else {
       throw new RuntimeException("Not unique: "+tmp)
     }
  }
  
  final def hasChildren : Boolean = childCount != 0
  
  final def hasNoChildren : Boolean = childCount == 0
  
  final def firstChild : IASTNode = children(0) 
  
  final def lastChild : IASTNode = children( children.size - 1 ) 
  
  def children : Seq[IASTNode]
  
  def parent : Option[IASTNode]
  
  def setParent(p : IASTNode ) : Unit
  
  def print(depth:Int) : String
  
  def visitInOrder( visitor : IASTNode => Unit ) : Unit
  
  def visitDepthFirst( visitor : IASTNode => Unit ) : Unit
  
  override def toString() : String = getClass.getSimpleName
  
  def evaluate() : TypedValue
}