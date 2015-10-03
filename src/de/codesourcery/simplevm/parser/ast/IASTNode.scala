package de.codesourcery.simplevm.parser.ast

import de.codesourcery.simplevm.parser.Scope

trait IASTNode extends Iterable[IASTNode]
{
  def scope : Option[Scope]
  
  def childCount : Int
  
  def addChild(child:IASTNode) : IASTNode
  
  def child(idx:Int) : IASTNode
  
  def children : Seq[IASTNode]
  
  def parent : Option[IASTNode]
  
  def setParent(p : IASTNode ) : Unit
  
  def print(depth:Int) : String
  
  def visitInOrder( visitor : IASTNode => Unit )
  
  override def toString() : String = getClass.getSimpleName
}