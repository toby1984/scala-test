package de.codesourcery.simplevm.compiler

trait ICompilationParticipant 
{
  def visit(ctx:ICompilationContext) : Unit  
}