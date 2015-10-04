package de.codesourcery.simplevm.compiler

class Opcode(val opcode:Int,val mnemonic:String) 
{
   def getBytes() : Array[Int] = Array[Int]( opcode )
   
   override def toString() : String = mnemonic
}

object Opcode 
{
   def LOAD_CONST(slotIndex:Int) : Opcode = new Opcode(1,"LOAD_CONST #"+slotIndex) 
   { 
      override def getBytes() : Array[Int] = Array[Int]( opcode , slotIndex )
   }
   
   val POP : Opcode = new Opcode(2,"POP")
   
   def store(slotIndex:Int) : Opcode = new Opcode(3,"STORE #"+slotIndex) { 
      override def getBytes() : Array[Int] = Array[Int]( opcode , slotIndex )
   }  
   
   val JUMP : Opcode = new Opcode(4,"JUMP") 
   
   val ADD : Opcode = new Opcode(5,"ADD")
   
   val SUB : Opcode = new Opcode(6,"SUB")
   
   val RETURN : Opcode = new Opcode(7,"RETURN")
   
   def JSR(slotIndex:Int) : Opcode = new Opcode(8,"JSR #"+slotIndex) 
   { 
      override def getBytes() : Array[Int] = Array[Int]( opcode , slotIndex )
   }
   
   def LOAD_VARIABLE(slotIndex:Int) : Opcode = new Opcode(9,"LOAD_VAR #"+slotIndex) { 
      override def getBytes() : Array[Int] = Array[Int]( opcode , slotIndex )
   }  
   
   def LOAD_ADDRESS(slotIndex:Int) : Opcode = new Opcode(10,"LOAD_ADDRESS #"+slotIndex) { 
      override def getBytes() : Array[Int] = Array[Int]( opcode , slotIndex )
   }    
}