package de.codesourcery.simplevm.vm

import de.codesourcery.simplevm.compiler.BinaryFile

class VirtualMachine(file:BinaryFile) 
{
  val constantPool = file.getConstantPool()
  val instructions = file.getInstructions()
  val jumpTable = file.getJumpTable()._1
  val methodSignatures = file.getJumpTable()._2
  
  private[this] var instructionPtr = 0
  
  def execute() : Unit = 
  {
     instructionPtr = 0    
  } 
}