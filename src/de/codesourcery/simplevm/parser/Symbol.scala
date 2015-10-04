package de.codesourcery.simplevm.parser

import de.codesourcery.simplevm.parser.ast.IASTNode

class Symbol(val name:Identifier,val symbolType:SymbolType) 
{
  if ( name == null ) {
    throw new IllegalArgumentException("name must not be NULL")
  }
  if ( symbolType == null ) {
    throw new IllegalArgumentException("symbolType must not be NULL")
  }
  override def toString() : String =  name+" , type: "+symbolType
}

class ValueSymbol(name:Identifier,symbolType:SymbolType,val node:IASTNode,val isImmutable:Boolean) extends Symbol(name,symbolType) 

class LabelSymbol(name:Identifier,symbolType:SymbolType,val node:IASTNode) extends Symbol(name,symbolType) 