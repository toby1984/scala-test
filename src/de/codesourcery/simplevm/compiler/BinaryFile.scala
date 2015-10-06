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

class BinaryFile 
{
  private[this] val FILE_VERSION = 1
  
  private val chunks = new ListBuffer[Chunk]()
  
  def setJumpTable(jumpTable:Seq[Int]) : Unit = 
  {
    addChunk(ChunkType.JUMP_TABLE) 
    {
      ser => ser.writeIntArray( jumpTable )
    }
  }
  
  def getJumpTable() : Seq[Int] = 
  {
    val chunk = getChunk(ChunkType.JUMP_TABLE)
    new Serializer(null,chunk.asInputStream).readIntArray()
  }
  
  def setConstantPool(constants:Seq[ConstantEntry]) : Unit = 
  {
    addChunk(ChunkType.CONSTANT_POOL) 
    {
      ser => constants.foreach( ser.writeConstantEntry )  
    }    
  }
  
  def getConstantPool() : Seq[ConstantEntry] = 
  {
    val chunk = getChunk(ChunkType.CONSTANT_POOL)
    val ser = new Serializer(null,chunk.asInputStream)
    
    val result = ListBuffer[ConstantEntry]()
    for ( ser <- ser.readConstantEntry() ) {
      result += ser
    }
    result
  }
  
  def getChunk(kind:ChunkType) : Chunk = chunks.filter( t => t.kind == kind ).head

  def setInstructions(instructions:Seq[Opcode]) : Unit = 
  {
    val out = new ListBuffer[Int]()
    instructions.foreach( _.append( out ) )
    
    addChunk(ChunkType.INSTRUCTIONS) 
    {
      ser => ser.writeIntArray( out )
    }    
  }
  
  def getInstructions() : Seq[Opcode] = 
  {
    val chunk = getChunk(ChunkType.INSTRUCTIONS)
    val ser = new Serializer(null,chunk.asInputStream)
    return Opcode.read( ser.readIntArray() )
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
  
  private[this] def addChunk( kind:ChunkType)(func : Serializer => Unit ) : Unit = 
  {
    val chunk = new Chunk( kind , FILE_VERSION )
    val out = new ByteArrayOutputStream
    val serializer = new Serializer(out,null)
    chunk.data = out.toByteArray()
    replaceOrAdd(chunk)
  }
  
  def addChunk( chunk : Chunk) : Unit = 
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
       ser.writeInt( kind.getTypeId() )
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
     val chunkType = ChunkType.fromTypeId( ser.readInt() )
     if ( chunkType != -1 ) 
     {
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
    for ( chunk <- Chunk.readFrom(ser) ) {
      result.addChunk(chunk)
    }
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
  
  def writeByteArray(value:Seq[Byte]) : Unit =
  {
    writeInt( value.length )
    value.foreach( output.write( _ ) )
  }
  
  def writeInt(value:Int) : Unit = 
  {
    output.write( (value >> 24 ) & 0xff )
    output.write( (value >> 16 ) & 0xff )
    output.write( (value >> 8 ) & 0xff )
    output.write( value & 0xff )
  }
  
  def readByteArray(len:Int) : Array[Byte] = 
  {
    val result = new Array[Byte](len)
    if ( input.read( result ) != len ) {
      throw new EOFException()
    }
    result
  }
  
  def readInt() : Int = 
  {
    var result = readSafely() << 8
    result = (result | readSafely() ) << 8
    result = (result | readSafely() ) << 8
    result = (result | readSafely() )    
    result
  }
  
  private[this] def readSafely() : Int = 
  {
    val tmp = readInt()
    if ( tmp == -1 ) {
      throw new EOFException()
    }   
    tmp
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
  
  def readConstantEntry() : Option[ ConstantEntry ] = 
  {
    val typeId = input.read()
    if ( typeId == -1 ) {
      None
    }
    else 
    {
      val kind = fromTypeId( typeId )  
      val value = kind match 
      {
        case KnownTypes.INTEGRAL => readInt()
        case KnownTypes.STRING => readString()
        case _ => throw new RuntimeException("Unhandled constant type: "+kind)
      }
      Some( ConstantEntry(value,kind) )
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