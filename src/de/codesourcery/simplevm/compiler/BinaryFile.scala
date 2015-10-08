package de.codesourcery.simplevm.compiler

import de.codesourcery.simplevm.compiler.Compiler._
import java.io.OutputStream
import java.io.InputStream
import scala.collection.mutable.ListBuffer
import java.io.ByteArrayOutputStream
import de.codesourcery.simplevm.parser.KnownTypes
import java.io.EOFException
import de.codesourcery.simplevm.compiler.BinaryFile._
import java.io.ByteArrayInputStream
import de.codesourcery.simplevm.parser.TypeName
import de.codesourcery.simplevm.compiler.Compiler._
import de.codesourcery.simplevm.parser.Identifier
import de.codesourcery.simplevm.Misc

class BinaryFile 
{
  private[this] val FILE_VERSION = 1
  
  private val chunks = new ListBuffer[Chunk]()
  
  /**
   * 
   * jumpTable  - list of indexes into the instruction/opcode area, pointing to the start of each function
   *              If an index is -1 , this is an external function that needs to be resolved by the VM during runtime
   * signatures - list of function signatures for each jumpTable entry (first jumpTable entry belongs to first signature, second entry to second signature,etc.)
   * frames     - Stack frames for each method 
   */
  
  def setJumpTable(stackFrames : Seq[Compiler#VariablesTable] , instructionPtrs:Seq[Int] , descriptors:Seq[JumpTableEntry] , globalVars:Compiler#VariablesTable) : Unit = 
  {
    addChunk(ChunkType.JUMP_TABLE) 
    {
      ser => 
        {
          ser.writeInt( stackFrames.size )
          for ( varTable <- stackFrames ) 
          {
            ser.writeVarEntries( varTable.sortedEntries )
          }
          ser.writeIntArray( instructionPtrs )
          ser.writeInt( descriptors.size )
          descriptors.foreach( ser.writeJumpTableEntry )
          ser.writeVarEntries( globalVars.varSlots.values.toSeq )
      }
    }
  }
  
  def getJumpTable() : (Seq[Seq[VarEntry]],Seq[Int],Seq[JumpTableEntry],Seq[VarEntry]) = 
  {
    readChunk(ChunkType.JUMP_TABLE) { ser => 
      {
        val stackFrames = new ListBuffer[Seq[VarEntry]]()
        val stackFrameCount = ser.readInt()
        for ( i <- 0 until stackFrameCount ) 
        {
          stackFrames += ser.readVarEntries()
        }
        val instructionPtrs = ser.readIntArray()
        val len = ser.readInt()
        val descriptors = for ( i <- 0 until len ) yield ser.readJumpTableEntry()
        val globalVars = ser.readVarEntries()
        (stackFrames,instructionPtrs , descriptors,globalVars)
      }
    }
  }
  
  def setConstantPool(constants:Seq[ConstantEntry]) : Unit = 
  {
    addChunk(ChunkType.CONSTANT_POOL) 
    {
      ser => 
        {
          println("Writing "+constants.size+" constant entries")
          ser.writeInt( constants.size )
          constants.foreach( ser.writeConstantEntry )  
        }
    }    
  }
  
  def getConstantPool() : Seq[ConstantEntry] = 
  {
    readChunk( ChunkType.CONSTANT_POOL) 
    { 
      ser => 
      {
    	  val count = ser.readInt()
    	  println("Reading "+count+" constant entries")
        for ( i <- 0 until count ) yield  ser.readConstantEntry()
      }
    }
  }
  
  private[this] def readChunk[T](kind:ChunkType)( func: Serializer => T) : T = 
  {
    println("Trying to read chunk "+kind+" ...")
    func( new Serializer(null,getChunk( kind ).asInputStream) )
  }
  
  private[this] def getChunk(kind:ChunkType) : Chunk = 
  {
    chunks.filter( t => t.kind == kind ).headOption match {
      case Some(x) => x
      case None => throw new RuntimeException("Failed to find chunk "+kind+" in file");
    }
  }

  def setInstructions(instructions:Seq[Opcode]) : Unit = 
  {
    addChunk(ChunkType.INSTRUCTIONS) 
    {
    	ser => {
    		val out = new ListBuffer[Int]()
    	  ser.writeIntArray( out )
        instructions.foreach( ins => out += ins.toBinary() )
    	}
    }    
  }
  
  def getInstructions() : Seq[Opcode] = 
  {
    readChunk( ChunkType.INSTRUCTIONS ) { ser => Opcode.read( ser.readIntArray() ) }
  }
  
  def writeTo(out:OutputStream) : Unit =
  {
     if ( ! chunks.exists( _.kind == ChunkType.FILE_HEADER) ) 
     {
       addChunk(ChunkType.FILE_HEADER) 
       {
         ser => ser.writeInt(0xdeadbeef)  
       }
     }
     val ser = new Serializer(out,null)
     chunks.sortBy( chunk => chunk.kind.getTypeId ).foreach( _.writeTo( ser ) )
  }
  
  private[this] def addChunk(kind:ChunkType)(func : Serializer => Unit ) : Unit = 
  {
    println("Adding chunk "+kind)
    val chunk = new Chunk( kind , FILE_VERSION )
    val out = new ByteArrayOutputStream
    val serializer = new Serializer(out,null)
    func(serializer)
    chunk.data = out.toByteArray()
    replaceOrAdd(chunk)
  }
  
  private[BinaryFile] def addChunk( chunk : Chunk) : Unit = 
  {
     if ( chunks.exists( _.kind == chunk.kind ) ) {
       throw new IllegalArgumentException("Duplicate chunk "+chunk)
     }
     chunks += chunk
  }
  
  private[this] def replaceOrAdd( chunk : Chunk) : Unit = 
  {
    var idx = 0
    while ( idx < chunks.size) 
    {
      val current = chunks(idx)
      if ( current.kind == chunk.kind ) 
      { 
        chunks.update( idx , chunk )
        return
      } 
        idx += 1
    }
    chunks += chunk
  }
}

class Chunk(val kind:ChunkType,val version:Int) 
{
   var data : Array[Byte] = new Array[Byte](0)
   
   def writeTo(ser:Serializer) : Unit = 
   {
     println(" Writing chunk "+kind+" with ID "+kind.getTypeId+" , version "+version+" and length "+data.length)
     ser.writeByte( kind.getTypeId().asInstanceOf[Byte] )
     ser.writeInt( version )
     ser.writeInt( data.length )
     ser.writeByteArray( data )
   }
   
   def asInputStream : InputStream = new ByteArrayInputStream(data)
}

object Chunk 
{
  def readFrom(ser:Serializer) : Option[Chunk] = 
  {
     val chunkTypeId = ser.unsafeReadByte()
     if ( chunkTypeId != -1 ) 
     {
    	 val chunkType = ChunkType.fromTypeId( chunkTypeId )
    	 println("File contains chunk "+chunkType)
    	 
       val version = ser.readInt()
       val result = new Chunk( chunkType , version)
       val len = ser.readInt()
       println("Reading "+len+" bytes")
       result.data = ser.readByteArray(len)
       Some(result)
     } else {
       None
     }
  }
}

object BinaryFile 
{
  def readFrom(in:InputStream) : BinaryFile = 
  {
    val ser = new Serializer(null,in)
    val result = new BinaryFile()
    Misc.repeatUntilNone( Chunk.readFrom(ser) ).foreach( result.addChunk( _ ) )
    result
  }
}

protected class Serializer(output: OutputStream,input:InputStream) 
{
  def writeIntArray(value:Seq[Int]) : Unit = 
  {
    writeInt( value.length )
    value.foreach( writeInt )
  }
  
  def readIntArray() : Array[Int] = 
  {
    val len = readInt()
    val result = new Array[Int](len)
    for ( i <- 0 until len ) 
    {
      result(i) = readInt()
    }
    result
  }
  
  def writeJumpTableEntry(entry:JumpTableEntry) : Unit = 
  {
    // sealed case class JumpTableEntry(val name:Identifier,val signature:Option[ FunctionSignature ],val slotIndex:Int) 
    writeIdentifier( entry.name )
    if ( entry.isFunction ) 
    {
      writeInt(1)
      writeFunctionSignature( entry.signature.get )
    } else {
      writeInt(2)
    }
    
    writeInt( entry.slotIndex )
  }
  
  def readJumpTableEntry() : JumpTableEntry = 
  {
    val name = readIdentifier()
    val kind = readInt()
    val signature = kind match 
    {
      case 1 => Some( readFunctionSignature() )
      case 2 => None
      case x => throw new RuntimeException("Unknown jump table entry type "+x)
    }
    val slotIndex = readInt()
    new JumpTableEntry(name,signature,slotIndex)
  }

  def writeFunctionDescriptor(desc:FunctionDescriptor) : Unit = 
  {
    writeFunctionSignature( desc.signature )
    writeInt( desc.slotIndex )
    writeInt( desc.firstInstructionIdx )
    writeVarEntries( desc.stackLayout )
    //  FunctionDescriptor(signature:FunctionSignature,slotIndex:Int,firstInstructionIdx:Int,stackLayout:Seq[VarEntry])
    // val name:Identifier,val params:Seq[ Seq[TypeName] ],val returnType:TypeName
  }
  
  def readFunctionDescriptor() : FunctionDescriptor = {
    val signature = readFunctionSignature()
    val slotIndex = readInt()
    val firstInstructionIdx = readInt()
    val stackLayout = readVarEntries()
    new FunctionDescriptor( signature , slotIndex , firstInstructionIdx , stackLayout )
  }
  
  private def writeFunctionSignature(sig:FunctionSignature) : Unit = 
  {
    writeIdentifier( sig.name )
    writeInt( sig.params.size )
    sig.params.foreach( writeTypeNames ) 
    writeTypeName( sig.returnType )
  }
  
  private def readFunctionSignature() : FunctionSignature = 
  {
      val name = readIdentifier()
      val len = readInt()
      val params = for ( i <- 0 until len ) yield readTypeNames()
      val returnType = readTypeName()
      new FunctionSignature(name,params,returnType)
  }
  
  private def writeIdentifier(id:Identifier) : Unit = writeString( id.name )
  
  private def readIdentifier() : Identifier = Identifier( readString() )
  
  private def writeTypeName(name:TypeName) : Unit = {
    writeString( name.name )
  }
  
  private def readTypeName() : TypeName = TypeName( readString() )
  
  private def writeTypeNames(names:Seq[TypeName]) : Unit = 
  {
    writeStringArray( names.map( name => name.name ) )
  }
  
  private def readTypeNames() : Seq[TypeName] = readStringArray().map( TypeName )
  
  def writeStringArray(value:Seq[String]) : Unit = 
  {
    writeInt( value.length )
    value.foreach( writeString )
  }  
  
  def readStringArray() : Seq[String] =
  {
    val len = readInt()
    for ( i <- 0 until len ) yield readString()
  }   
  
  def writeVarEntries( entries : Seq[VarEntry ] ) : Unit = 
  {
    writeInt( entries.length )
    entries.sortBy(entry => entry.slotNum ).foreach( writeVarEntry )
  }
  
  def unsafeReadByte() : Int = input.read()
  
  private[this] def writeVarEntry( entry : VarEntry ) : Unit = 
  {
    writeString( entry.name.name )
    writeString( entry.kind.name )
  }
  
  def readVarEntries() : Seq[VarEntry ] = {
    val len = readInt()
    for ( i <- 0 until len ) yield readVarEntry(i)
  }
  
  private[this] def readVarEntry(slotIndex:Int) : VarEntry  = {
    val name = readString()
    val kind = TypeName( readString() )
    VarEntry(Identifier(name),kind,slotIndex)
  }
  
  def writeByteArray(value:Seq[Byte]) : Unit =
  {
    writeInt( value.length )
    output.write( value.toArray )
  }
  
  def writeByte(value:Byte) : Unit = {
    output.write( value )
  }
  
  def writeInt(value:Int) : Unit = 
  {
    output.write( (value >> 24 ) & 0xff )
    output.write( (value >> 16 ) & 0xff )
    output.write( (value >> 8 ) & 0xff )
    output.write( value & 0xff )
  }
  
  def readInt() : Int = ( readSafely() << 24 | readSafely() << 16 | readSafely() << 8 | readSafely() )
  
  def readByteArray(len:Int) : Array[Byte] = 
  {
    val result = new Array[Byte](len)
    if ( input.read( result ) != len ) {
      throw new EOFException()
    }
    result
  }
  
  private def readUnsafe() : Int = input.read()
  
  private[this] def readSafely() : Int = readUnsafe() match 
  {
    case -1 => throw new EOFException()
    case x => x
  }
  
  def writeConstantEntry(entry:ConstantEntry) : Unit = 
  {
    writeInt( getTypeId(entry) )
    entry.kind match 
    {
      case KnownTypes.INTEGRAL => writeInt( entry.value.asInstanceOf[Number].intValue() )
      case KnownTypes.STRING =>  writeString( entry.value.asInstanceOf[String] )
      case _ => throw new RuntimeException("Unhandled constant type: "+entry.kind)
    }
  }
  
  private[this] def getTypeId(entry:ConstantEntry) : Int = 
  {
    entry.kind match 
    {
      case KnownTypes.INTEGRAL => 0
      case KnownTypes.STRING => 1
      case _ => throw new RuntimeException("Unhandled constant type: "+entry.kind)
    }
  }
  
  private[this] def fromTypeId(typeId:Int) : TypeName = 
  {
    typeId match {
      case 0 => KnownTypes.INTEGRAL
      case 1 => KnownTypes.STRING
      case _ => throw new RuntimeException("Unhandled ConstantEntry type ID: "+typeId)
    }
  }  
  
  def readConstantEntry() : ConstantEntry = 
  {
    val typeId = input.read()
    val kind = fromTypeId( typeId )  
    val value = kind match 
    {
      case KnownTypes.INTEGRAL => readInt()
      case KnownTypes.STRING => readString()
      case _ => throw new RuntimeException("Unhandled constant type: "+kind)
    }
    ConstantEntry(value,kind)
  }
  
  def readString() : String = 
  {
    val len = readSafely()
    val array = new Array[Char](len)
    for ( i <- 0 until len ) 
    {
      array(i) = readSafely().asInstanceOf[Char]
    }
    new String(array)
  }
  
  def writeString(value:String) : Unit = 
  {
     writeInt( value.length )
     for ( c <- value.toCharArray() ) 
     {
       writeInt( c )
     }
  }
}