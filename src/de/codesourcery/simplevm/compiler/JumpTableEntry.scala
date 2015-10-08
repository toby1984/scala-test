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
sealed class JumpTableEntry(val name:Identifier,val signature:Option[ FunctionSignature ],val slotIndex:Int) 
{
  // entries may either be function names OR auto-generated
  // local labels inside a block (used for implementing control-flow instructions)
  def isFunction : Boolean = signature.isDefined
  
  def isLocalLabel : Boolean = ! signature.isDefined
  
  def argumentCount : Int = if ( signature.isDefined ) signature.get.argumentCount else 0
  
  override def equals(obj:Any) : Boolean = 
  {
    if ( obj.isInstanceOf[JumpTableEntry] ) 
    {
      val other = obj.asInstanceOf[JumpTableEntry]
      if ( other.name != this.name ) {
        return false
      }
      if ( this.signature.isDefined != other.signature.isDefined ) {
        return false
      }
      if ( this.signature.isDefined && this.signature.get != other.signature.get ) {
        return false
      }
      true
    } else {
      false
    }      
  }
  
  override def hashCode() : Int = 31*name.name.hashCode() + ( if ( signature.isEmpty ) 0 else signature.get.hashCode )
  
  if ( name == null ) {
    throw new IllegalArgumentException("Name cannot be NULL")
  }
}