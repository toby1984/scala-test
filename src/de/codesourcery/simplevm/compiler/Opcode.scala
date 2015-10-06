package de.codesourcery.simplevm.compiler

import scala.collection.mutable.ListBuffer

class Opcode(val opcode:Int,val slotIndex:Int,val mnemonic:String) 
{
   def toBinary() : Array[Int] = Array[Int]( opcode )
   
   def this(opcode:Int,mnemonic:String) 
   {
     this(opcode,0,mnemonic)
   }
   
   final def append(out:ListBuffer[Int]) : Unit = 
   {
     val tmp = toBinary()
     var ptr = 0
     while( ptr < tmp.length ) {
       out += tmp(ptr)
       ptr += 1
     }
   }
   
   override def toString() : String = mnemonic
}

object Opcode 
{
   def LOAD_CONST(slotIndex:Int) : Opcode = new Opcode(1,"LOAD_CONST #"+slotIndex) 
   { 
      override def toBinary() : Array[Int] = Array[Int]( opcode , slotIndex )
   }
   
   val POP : Opcode = new Opcode(2,"POP")
   
   def store(slotIndex:Int) : Opcode = new Opcode(3,"STORE #"+slotIndex) { 
      override def toBinary() : Array[Int] = Array[Int]( opcode , slotIndex )
   }  
   
   val ADD : Opcode = new Opcode(5,"ADD")
   
   val SUB : Opcode = new Opcode(6,"SUB")
   
   val RETURN : Opcode = new Opcode(7,"RETURN")
   
   def JSR(slotIndex:Int) : Opcode = new Opcode(8,"JSR #"+slotIndex) 
   { 
      override def toBinary() : Array[Int] = Array[Int]( opcode , slotIndex )
   }
   
   def LOAD_VARIABLE(slotIndex:Int) : Opcode = new Opcode(9,"LOAD_VAR #"+slotIndex) { 
      override def toBinary() : Array[Int] = Array[Int]( opcode , slotIndex )
   }  
   
   def LOAD_ADDRESS(slotIndex:Int) : Opcode = new Opcode(10,"LOAD_ADDRESS #"+slotIndex) { 
      override def toBinary() : Array[Int] = Array[Int]( opcode , slotIndex )
   }    
   
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
        val opCode = readValue() match 
        {
          case  1 => LOAD_CONST( readValue() )
          case  2 => POP
          case  3 => store( readValue() )
          case  5 => ADD
          case  6 => SUB
          case  7 => RETURN
          case  8 => JSR( readValue() )
          case  9 => LOAD_VARIABLE( readValue() )
          case 10 => LOAD_ADDRESS( readValue() )
          case x => throw new RuntimeException("Unknown opcode #"+x)
        }
        result += opCode
      }
      result
   }
}