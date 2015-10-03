package de.codesourcery.simplevm.parser

import de.codesourcery.simplevm.parser.ast.IASTNode

sealed case class Symbol(name:Identifier,symbolType:SymbolType,node:IASTNode) 
{
  
}