package de.codesourcery.simplevm.compiler

import de.codesourcery.simplevm.parser.ast.AST
import de.codesourcery.simplevm.parser.ast.OperatorNode
import de.codesourcery.simplevm.parser.OperatorType
import de.codesourcery.simplevm.parser.ast.IASTNode
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import de.codesourcery.simplevm.parser.ast.FunctionDefinition
import de.codesourcery.simplevm.parser.ast.ParameterList
import de.codesourcery.simplevm.parser.ast.FunctionArgument

class Compiler 
{
  private[this] def filter[T <: IASTNode]( clazz : Class[T] )(nodes: Iterable[IASTNode]) : Iterable[T] = {
    nodes.filter( _.getClass == clazz ).map( _.asInstanceOf[ T ] )
  }
  
  def compile( ast : AST) 
  {
     validate( ast )
  }
  
  private[this] def validate(ast:AST) 
  {
     for ( funcNode <- filter( classOf[ FunctionDefinition ] )(ast) )
     {
       for ( paramList <- filter( classOf[ ParameterList ] )(funcNode) ) 
       {
         for ( arg <- filter( classOf[ FunctionArgument] )(paramList) )
         {
           if ( ! arg.kind.isKnownType ) {
             throw new RuntimeException("Unsupported type: "+arg.kind)
           }
         }
       }
     }
  }
}