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
        ser.writeArray( descriptors )( ser.writeJumpTableEntry ) 
        ser.writeArray( stackFrames )( x => ser.writeVarEntries( x.sortedEntries ) )
        ser.writeIntArray( instructionPtrs )
        ser.writeVarEntries( globalVars.varSlots.values.toSeq )
      }
    }
  }
  
  def getJumpTable() : (Seq[Seq[VarEntry]],Seq[Int],Seq[JumpTableEntry],Seq[VarEntry]) = 
  {
    readChunk(ChunkType.JUMP_TABLE) { ser => 
      {
        val descriptors = ser.readArray( ser.readJumpTableEntry ).sortBy( entry => entry.slotIndex )
        val stackFrames = ser.readArray( ser.readVarEntries )
        val instructionPtrs = ser.readIntArray()
        val globalVars = ser.readVarEntries()
        (stackFrames,instructionPtrs , descriptors,globalVars)
      }
    }
  }
  
  def setConstantPool(constants:Seq[ConstantEntry]) : Unit = 
  {
    addChunk(ChunkType.CONSTANT_POOL) 
    {
      ser => ser.writeArray( constants )( ser.writeConstantEntry )
    }    
  }
  
  def getConstantPool() : Seq[ConstantEntry] = 
  {
    readChunk( ChunkType.CONSTANT_POOL) 
    { 
      ser => ser.readArray( ser.readConstantEntry )
    }
  }
  
  private[this] def readChunk[T](kind:ChunkType)( func: Serializer => T) : T = 
  {
    val ser = new Serializer(null,getChunk( kind ).asInputStream)
    try {
      func( ser )
    }
    catch 
    {
      case ex : Exception => 
      {
        println("Failed at offset "+ser)
        throw ex
      }
    }
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
    	ser => ser.writeIntArray( instructions.map( _.toBinary() ) )
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
  
  private[this] def addChunk(kind:ChunkType)(func : Serializer => Unit ) : Chunk = 
  {
    val chunk = new Chunk( kind , FILE_VERSION )
    val out = new ByteArrayOutputStream
    val serializer = new Serializer(out,null)
    func(serializer)
    out.close()
    chunk.data = out.toByteArray()
    replaceOrAdd(chunk)
    chunk
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

final class Chunk(val kind:ChunkType,val version:Int) 
{
   var data = new Array[Byte](0)
   
   def writeTo(ser:Serializer) : Unit = 
   {
     ser.writeByte( kind.getTypeId().asInstanceOf[Byte] )
     ser.writeInt( version )
     ser.writeByteArray( data )
   }
   
   def toHexDump : String = BinaryFile.toHexDump( this.data )
   
   override def toString() : String = "Chunk "+kind+", version "+version+" , length "+data.length
   
   def asInputStream : InputStream = new ByteArrayInputStream(data)
}

object Chunk 
{
  def readFrom(ser:Serializer) : Option[Chunk] = 
  {     
     val chunkTypeId = ser.readUnsafe()
     if ( chunkTypeId != -1 ) 
     {
    	 val chunkType = try {
    	   ChunkType.fromTypeId( chunkTypeId & 0xff )
    	 } 
    	 catch 
    	 {
    	   case ex : Exception => 
    	   {
    	     println( ser )
    	     throw ex
    	   }
    	 }
    	 
       val version = ser.readInt()
       val result = new Chunk( chunkType , version)
       val len = ser.readInt()

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
  
   private[this] def toHex(value:Int,padLen : Int) : String = {
     var result = Integer.toHexString( value )
     while ( result.length() < padLen ) {
       result = "0"+result
     }
     result
   }
   
   def toHexDump(data:Array[Byte]) : String = 
   {
     val result = new StringBuilder
     val hexLine = new StringBuilder
     val asciiLine = new StringBuilder
     
     var adr = 0
     val bytesPerLine = 16
     while ( adr < data.length ) 
     {
       if ( (adr % bytesPerLine) ==  0) {
           result.append( hexLine.toString ).append(" ").append( asciiLine.toString ).append("\n")         
           hexLine.setLength(0)
           asciiLine.setLength(0)
           hexLine.append( toHex( adr , 4 ) ).append(": ")
       }
       var i = 0
       while ( i < bytesPerLine && adr < data.length ) 
       {
         val v = data(adr).asInstanceOf[Int] & 0xff
         hexLine.append( toHex( v , 2 ) ).append(" ") 
         if ( v >= 32 && v < 127 ) {
           asciiLine.append( v.asInstanceOf[Char] )
         } else {
           asciiLine.append( '.' )
         }
         adr += 1
         i += 1
       }
     }
     result.append( hexLine.toString ).append(" ").append( asciiLine.toString )      
     result.toString()
   }  
}

protected class Serializer(output: OutputStream,inStream:InputStream) 
{
  private var readPtr = 0
  private var writePtr = 0
  
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
    val result = new JumpTableEntry(name,signature,slotIndex)
    result
  }

  def writeFunctionDescriptor(desc:FunctionDescriptor) : Unit = 
  {
    writeFunctionSignature( desc.signature )
    writeInt( desc.slotIndex )
    writeInt( desc.firstInstructionIdx )
    writeVarEntries( desc.stackLayout )
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
    writeArray( sig.params )( writeTypeNames )
    writeTypeName( sig.returnType )
  }
  
  private def readFunctionSignature() : FunctionSignature = 
  {
      val name = readIdentifier()
      val params = readArray( readTypeNames )
      val returnType = readTypeName()
      new FunctionSignature(name,params,returnType)
  }
  
  private def writeIdentifier(id:Identifier) : Unit = writeString( id.name )
  
  private def readIdentifier() : Identifier = Identifier( readString() )
  
  private def writeTypeName(name:TypeName) : Unit = writeString( name.name )
  
  private def readTypeName() : TypeName = TypeName( readString() )
  
  private def writeTypeNames(names:Seq[TypeName]) : Unit =  writeStringArray( names.map( name => name.name ) )
  
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
  
  private[this] def writeVarEntry( entry : VarEntry ) : Unit = 
  {
    writeIdentifier( entry.name )
    writeTypeName( entry.kind )
  }
  
  private[this] def readVarEntry(slotIndex:Int) : VarEntry  = {
    val name = readIdentifier()
    val kind = readTypeName()
    VarEntry(name,kind,slotIndex)
  }  
  
  def readVarEntries() : Seq[VarEntry ] = {
    val len = readInt()
    for ( i <- 0 until len ) yield readVarEntry(i)
  }
  
  def writeByteArray(value:Seq[Byte]) : Unit =
  {
    writeInt( value.length )
    output.write( value.toArray )
  }
  
  def writeByte(value:Byte) : Unit = {
    output.write( value )
  }
  
  def readArray[T]( func : => () => T ) : Seq[T] = {
    val len = readInt()
    for ( i <- 0 until len ) yield func()
  }
  
  def writeArray[T]( data : Seq[T])(func : => T => Unit ) : Unit = 
  {
    writeInt( data.size )
    data.foreach( func ) 
  }
  
  def writeInt(value:Int) : Unit = 
  {
    output.write( (value >> 24 ) & 0xff )
    output.write( (value >> 16 ) & 0xff )
    output.write( (value >> 8 ) & 0xff )
    output.write( value & 0xff )
  }
  
  def readInt() : Int = 
  {
    val v1 = readByteSafely() << 24 
    val v2 = readByteSafely() << 16 
    val v3 = readByteSafely() <<  8 
    val v4 = readByteSafely()  
    v1|v2|v3|v4
  }
  
  def readByteArray(len:Int) : Array[Byte] = 
  {
    val result = new Array[Byte](len)
    val actualLen = inStream.read(result)
    readPtr += ( if ( actualLen >= 0 ) actualLen else 0)
    if ( actualLen != len ) {
      throw new EOFException()
    }
    result
  }
  
  override def toString() : String = "Serializer[ readPtr @ "+readPtr+" , writePtr @ "+writePtr+"]"
  
  def readUnsafe() : Int = 
  {
    readPtr += 1    
    inStream.read()
  }
  
  private[this] def readByteSafely() : Int = readUnsafe() match 
  {
    case -1 => throw new EOFException()
    case x => x & 0xff
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
  
  private[this] def getTypeId(entry:ConstantEntry) : Int = entry.kind match 
  {
    case KnownTypes.INTEGRAL => 0
    case KnownTypes.STRING => 1
    case _ => throw new RuntimeException("Unhandled constant type: "+entry.kind)
  }
  
  def readConstantEntry() : ConstantEntry = 
  {
    val kind = fromTypeId( readInt() )  
    val value = kind match 
    {
      case KnownTypes.INTEGRAL => readInt()
      case KnownTypes.STRING => readString()
      case _ => throw new RuntimeException("Unhandled constant type: "+kind)
    }
    ConstantEntry(value,kind)
  }

  private[this] def fromTypeId(typeId:Int) : TypeName =  typeId match 
  {
    case 0 => KnownTypes.INTEGRAL
    case 1 => KnownTypes.STRING
    case _ => throw new RuntimeException("Unhandled ConstantEntry type ID: "+typeId)
  }
  
  def readString() : String = 
  {
    val len = readInt()
    val array = new Array[Byte](len)
    val actualLen = inStream.read( array )
    readPtr += ( if ( actualLen >= 0 ) actualLen else 0 )
    if ( actualLen != len ) {
      throw new EOFException()
    }
    new String(array)
  }
  
  def writeString(value:String) : Unit = 
  {
    val data = value.getBytes
    writeInt( data.length )
    output.write( data )
  }
}