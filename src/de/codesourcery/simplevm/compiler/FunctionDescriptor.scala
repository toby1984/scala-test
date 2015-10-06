package de.codesourcery.simplevm.compiler

sealed case class FunctionDescriptor(signature:FunctionSignature,slotIndex:Int,firstInstructionIdx:Int,stackLayout:Seq[VarEntry])