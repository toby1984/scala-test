package de.codesourcery.simplevm.compiler

import scala.collection.mutable.ListBuffer

case class Opcode(val opcode:Int,val mnemonic:String,val slotIndex:Int=0) 
{
   final def toBinary() : Int = (opcode << 24) | slotIndex;
   
   override def toString() : String = mnemonic
}

object Opcode 
{
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
   
   def read(in:Array[Int]) : Seq[Opcode] =  
   {
      var ptr = 0
      def readValue() : Int = {
        val result = in(ptr)
        ptr += 1
        result
      }
      val result = ListBuffer[Opcode]()
      while ( ptr < in.length) 
      {
        val input = readValue()
        val opcodePart = (input & 0xff000000) >>> 24
        val slotPart =    input & 0x00ffffff 
        
        val opCode = opcodePart match 
        {
          case  1 => LOAD_CONST( slotPart )
          case  2 => POP
          case  3 => STORE_STACK( slotPart )
          case  4 => STORE_HEAP( slotPart )
          case  5 => ADD
          case  6 => SUB
          case  7 => RETURN
          case  8 => JSR( slotPart )
          case  9 => LOAD_STACK( slotPart )
          case 10 => LOAD_HEAP( slotPart )
          case x => throw new RuntimeException("Unknown opcode #"+x)
        }
        result += opCode
      }
      result
   }
}