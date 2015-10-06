package de.codesourcery.simplevm.compiler

import de.codesourcery.simplevm.parser.Identifier

sealed case class JumpTableEntry(val name:Identifier,val signature:Option[ FunctionSignature ],val slotIndex:Int) 
{
  // entries may either be function names OR auto-generated
  // local labels inside a block (used for implementing control-flow instructions)
  def isFunction : Boolean = signature.isDefined
  
  def isLocalLabel : Boolean = ! signature.isDefined
}