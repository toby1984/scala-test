package de.codesourcery.simplevm.parser.ast

import scala.collection.mutable.ArrayBuffer
import de.codesourcery.simplevm.parser.Scope
import scala.collection.mutable.Stack

abstract class ASTNode extends IASTNode 
{
  private[this] val childs = new ArrayBuffer[IASTNode]
  private[this] var parentNode : Option[IASTNode] = None 
  
  override final def addChild(child: IASTNode): IASTNode = {
    childs += child    
    child.setParent( this )
    child
  }
  
  override final def child(idx:Int) : IASTNode = childs(idx)

  override final def childCount: Int = childs.size

  override final def children : Seq[IASTNode] = childs.toSeq
  
  override final def parent = parentNode
  
  override def setParent(p:IASTNode) : Unit = parentNode = Some(p)
  
  override def scope: Option[Scope] = parent.flatMap( p => p.scope )   
  
  final def visitInOrder( visitor : IASTNode => Unit ) {
    visitor( this )
    childs.foreach( _.visitInOrder( visitor ) )
  }
  
  final override def iterator : Iterator[IASTNode] = 
  {
    new Iterator[IASTNode]() 
    {
      private[this] val stack = new Stack[IASTNode]
      
      override def hasNext : Boolean = ! stack.isEmpty
      
      override def next : IASTNode = 
      {
        val result = stack.pop();
        stack.pushAll( result.children )
        result
      }
      stack.push( ASTNode.this )
    }
  }
}