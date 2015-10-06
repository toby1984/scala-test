package de.codesourcery.simplevm.compiler

import de.codesourcery.simplevm.parser.ValueSymbol
import de.codesourcery.simplevm.parser.LabelSymbol
import de.codesourcery.simplevm.parser.Symbol
import de.codesourcery.simplevm.parser.Scope
import de.codesourcery.simplevm.parser.Identifier
import de.codesourcery.simplevm.parser.TypeName
import de.codesourcery.simplevm.parser.ast.TypedValue

trait ICompilationContext 
{
     def currentPassNo : Int
     
     def pushScope(scope:Scope) : Unit
     
     def defineFunction( name : Identifier ): Unit
     
     def declareFunction( name : Identifier ): Unit
     
     def popScope() : Unit
     
     def currentScope : Scope
     
     def beginFunction(name:Identifier,scope:Scope) : Unit
     
     def endFunction() : Unit
     
     def registerConstant(symbol:ValueSymbol) : Unit
     
     def registerVariable(name:Identifier,kind:TypeName) : Unit
     
     def emitPop() : Unit // pop value from stack
     
     def emitLoad(value:TypedValue) : Unit// push value on stack
     
     def emitLoad(symbol:Symbol) : Unit// push value on stack
     
     def emitStore(symbol:ValueSymbol) : Unit// pop value from stack and store it at given location
     
     def emitJumpSubroutine(symbol:LabelSymbol) : Unit// pop value from stack and jump to this location
     
     def emitAdd() : Unit// pop two values from stack , add them and push result
     
     def emitSub() : Unit// pop two values from stack , subtract them and push result   
     
     def emitReturn() : Unit
}

final case class Label(val instructionOffset:Int)

final case class Slot(val offset:Int)