package de.codesourcery.simplevm.parser

import de.codesourcery.simplevm.parser.ast.IASTNode

sealed case class Symbol(name:Identifier,symbolType:SymbolType,node:IASTNode) 
{
  if ( name == null ) {
    throw new IllegalArgumentException("name must not be NULL")
  }
  if ( symbolType == null ) {
    throw new IllegalArgumentException("symbolType must not be NULL")
  }
}