package de.codesourcery.simplevm.vm

import de.codesourcery.simplevm.compiler.BinaryFile
import de.codesourcery.simplevm.compiler.VarEntry
import de.codesourcery.simplevm.compiler.JumpTableEntry
import de.codesourcery.simplevm.compiler.Opcode
import de.codesourcery.simplevm.compiler.ConstantEntry
import de.codesourcery.simplevm.compiler.Compiler
import scala.collection.mutable.Stack
import scala.collection.mutable.ListBuffer
import de.codesourcery.simplevm.parser.Identifier

class VirtualMachine(file:BinaryFile) 
{
  val MAX_DATA_STACK_SIZE : Int = 255
	val MAX_CALL_STACK_SIZE : Int = 255
  
  val constantPool : Seq[ConstantEntry] = file.getConstantPool()
  val instructions : Seq[Opcode] = file.getInstructions()
  
  val stackFrames : Seq[Seq[VarEntry]] = file.getJumpTable()._1
  val jumpTablePtrs : Seq[Int] = file.getJumpTable()._2
  val jumpTableEntries : Seq[JumpTableEntry] = file.getJumpTable()._3
  
  val globalVarMap : Seq[VarEntry] = file.getJumpTable()._4
  val globalVars = new Array[Any](globalVarMap.size)
  
  private[this] var framePointer = 0
  private[this] var dataStackPtr = 0
  private[this] var callStackPtr = 0
  private[this] var instructionPtr = 0
  
  private[this] val dataStack = new Array[Any]( MAX_DATA_STACK_SIZE )
  private[this] val callStack = new Array[Int]( MAX_CALL_STACK_SIZE )
  private[this] val framePointerStack = new Stack[Int]()
  
  def run() : Unit = 
  {
    // execute init method, this method will JSR to main() if it's present
    val initMethodSlotIdx = jumpTableEntries.filter( entry => entry.signature == Compiler.INIT_METHOD_IDENTIFIER ).headOption.map( _.slotIndex )
    if ( ! initMethodSlotIdx.isDefined ) 
    {
      throw new RuntimeException("No __init__ method ??")
    }
    jumpToSubroutine( initMethodSlotIdx.get )
    execute()
  }
  
  private[this] def jumpToSubroutine( slotIdx : Int ) : Unit = 
  {
     val signature = jumpTableEntries( slotIdx ).signature
     if ( ! signature.isDefined ) {
       throw new RuntimeException("enterMethod() called for non-function ??")
     }
     
     if ( jumpTablePtrs( slotIdx ) == -1 ) { // unresolved method, see if it's part of the runtime library
       
     }
     
     // set framepointer so that framePointer[0] is the first method argument
     framePointer = dataStackPtr - signature.get.argumentCount  
     
     // remember current data stack pointer
     framePointerStack.push( dataStackPtr )
     
     // increase stack so that (stackPtr - framePointer) = max. size of method stack (= arg count + local vars)
     dataStackPtr += stackFrames( slotIdx ).size
     
     // remember current instruction + 1 as return address
     callStack( callStackPtr ) = instructionPtr
     callStackPtr += 1
     
     // update instruction ptr
    instructionPtr = jumpTablePtrs( slotIdx )
  }
  
  private[this] def executeBuiltinFunction(slotIdx:Int) : Unit = 
  {
      val signature = jumpTableEntries(slotIdx).signature
      if ( signature.get.name == Identifier("print") ) 
      {
        println( "print( "+pop()+")" )  
      } 
      else 
      {
        throw new RuntimeException("Unknown function: "+signature.get)
      }
  }
  
  private[this] def exitMethod() : Unit = 
  {
    callStackPtr -= 1
    instructionPtr = callStack( callStackPtr )
    
    framePointer = framePointerStack.pop()
  }
  
  private[this] def execute() : Unit = 
  {
    var continue = false
    do 
    {
      val currentIns = instructions(instructionPtr)
      instructionPtr += 1
      continue = execute( currentIns )
    } while ( continue )
  } 
  
  private[this] def loadConstant(slotIndex:Int) : Any = push( constantPool(slotIndex).value )
  
	private[this] def readStackVariable(slotIndex:Int) : Any =  dataStack( framePointer + slotIndex)
  
  private[this] def push(value:Any) : Unit = 
  {
    dataStack( dataStackPtr ) = value
    dataStackPtr += 1
  }
  
  private[this] def pop() : Any = 
  {
		dataStackPtr -= 1
    dataStack(dataStackPtr)
  }
  
  private[this] def execute( ins:Opcode) : Boolean  = 
  {
   /*
   def LOAD_CONST(slotIndex:Int) : Opcode = new Opcode(1,"LOAD_CONST #"+slotIndex,slotIndex) 
   val POP : Opcode = new Opcode(2,"POP")
   def STORE_STACK(slotIndex:Int) : Opcode = new Opcode(3,"STORE_STACK #"+slotIndex,slotIndex)
   def STORE_HEAP(slotIndex:Int) : Opcode = new Opcode(4,"STORE_HEAP #"+slotIndex,slotIndex)
   val ADD : Opcode = new Opcode(5,"ADD")
   val SUB : Opcode = new Opcode(6,"SUB")
   val RETURN : Opcode = new Opcode(7,"RETURN")
   def JSR(slotIndex:Int) : Opcode = new Opcode(8,"JSR #"+slotIndex,slotIndex) 
   def LOAD_STACK(slotIndex:Int) : Opcode = new Opcode(9,"LOAD_STACK #"+slotIndex,slotIndex)
   def LOAD_HEAP(slotIndex:Int) : Opcode = new Opcode(10,"LOAD_HEAP #"+slotIndex,slotIndex)    
    */
    val slotIdx = ins.slotIndex
    ins.opcode match 
    {
      case 1 => loadConstant( slotIdx ) // LOAD_CONST
      case 2 => pop() // POP
      case 3 => dataStack( framePointer + slotIdx )=pop() // STORE_STACK
      case 4 => globalVars( slotIdx )=pop() // STORE_HEAP
      case 5 => push( pop().asInstanceOf[Number].longValue() + pop().asInstanceOf[Number].longValue() ) // ADD
      case 6 => push( pop().asInstanceOf[Number].longValue() - pop().asInstanceOf[Number].longValue() ) // SUB
      case 7 => exitMethod() // RETURN
      case 8 => jumpToSubroutine( slotIdx ) // JSR
      case 9 => push( readStackVariable( slotIdx ) ) // LOAD_STACK
      case 10 => push( globalVars( slotIdx ) ) // LOAD_HEAP
      case _ => throw new RuntimeException("Unknown instruction: "+ins)
    }
    return true
  }
}