package de.codesourcery.simplevm.parser

import de.codesourcery.simplevm.parser.ast.IASTNode

class Symbol(val scope:Scope,val name:Identifier,val symbolType:SymbolType) 
{
  if ( name == null ) {
    throw new IllegalArgumentException("name must not be NULL")
  }
  if ( symbolType == null ) {
    throw new IllegalArgumentException("symbolType must not be NULL")
  }
  override def toString() : String = scope.fullyQualifiedName( this )+" , type: "+symbolType
}

class ValueSymbol(scope:Scope,name:Identifier,symbolType:SymbolType,val node:IASTNode,val isImmutable:Boolean) extends Symbol(scope,name,symbolType) 

class LabelSymbol(scope:Scope,name:Identifier,symbolType:SymbolType,val node:IASTNode) extends Symbol(scope,name,symbolType) 