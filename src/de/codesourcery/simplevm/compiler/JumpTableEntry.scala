package de.codesourcery.simplevm.compiler

import de.codesourcery.simplevm.parser.Identifier

/**
 * An entry in a jump table.
 * 
 * A jump tables hold pointers into the instruction list, denoting the start of code blocks like functions,
 * loops etc.
 * 
 * @param name Label of this entry point
 * @param signature Function signature if this jump table entry describes the start of a function block
 * @param slotIndex ptr into the instruction buffer
 */
sealed case class JumpTableEntry(val name:Identifier,val signature:Option[ FunctionSignature ],val slotIndex:Int) 
{
  // entries may either be function names OR auto-generated
  // local labels inside a block (used for implementing control-flow instructions)
  def isFunction : Boolean = signature.isDefined
  
  def isLocalLabel : Boolean = ! signature.isDefined
}