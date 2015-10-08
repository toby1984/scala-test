package de.codesourcery.simplevm.compiler

import de.codesourcery.simplevm.parser.ast.AST
import de.codesourcery.simplevm.parser.ast.OperatorNode
import de.codesourcery.simplevm.parser.OperatorType
import de.codesourcery.simplevm.parser.ast.IASTNode
import scala.reflect.runtime.universe._
import scala.reflect.ClassTag
import de.codesourcery.simplevm.parser.ast.FunctionDefinition
import de.codesourcery.simplevm.parser.ast.ParameterList
import de.codesourcery.simplevm.parser.ast.FunctionArgument
import scala.collection.mutable.ListBuffer
import de.codesourcery.simplevm.parser.Symbol
import de.codesourcery.simplevm.parser.ValueSymbol
import de.codesourcery.simplevm.parser.LabelSymbol
import scala.collection.mutable.HashMap
import de.codesourcery.simplevm.parser.Scope
import de.codesourcery.simplevm.parser.ast.TypedValue
import scala.collection.mutable.Stack
import de.codesourcery.simplevm.parser.Identifier
import de.codesourcery.simplevm.parser.TypeName
import de.codesourcery.simplevm.parser.ast.FunctionDefinition
import de.codesourcery.simplevm.parser.KnownTypes
import de.codesourcery.simplevm.parser.SymbolType
import de.codesourcery.simplevm.parser.ast.FunctionDefinition
import java.io.ByteArrayOutputStream
import java.io.ByteArrayInputStream

final class Compiler 
{ 
  final class ConstantPool 
  {
    val values = new ListBuffer[ConstantEntry]()
    
    def slotOf(v : TypedValue) : Int = 
    {
       val value = v.value
       val kind = v.kind
       
       kind.assertKnownType()
       
       var index = 0;
       while ( index < values.size ) 
       {
         val entry = values(index)
         if ( entry.kind == kind && entry.value == value ) {
           return index
         }
         index += 1
       }
       val result = values.size
       value match 
       {
         case Some(x) => values += ConstantEntry(x,kind)
         case x => values += ConstantEntry(x,kind)
       }
       result
    }
  }
  
  final class JumpTable 
  {
    val insBuffer = new ListBuffer[Opcode]()    
    var jmpTableIndex = 0
    val insPtrs = new HashMap[Int,Int] // key is slot index, value is pointer into instruction buffer
    val labelToSlot = new HashMap[Identifier,JumpTableEntry]()
    val stackFrames = new ListBuffer[VariablesTable]()
    
    def getFunctionDescriptors : Seq[FunctionDescriptor] = 
    {
      val result = new ListBuffer[FunctionDescriptor]()
      // FunctionDescriptor(signature:FunctionSignature,slotIndex:Int,firstInstructionIdx:Int,stackLayout:Seq[VarEntry])
      for ( i <- 0 until jmpTableIndex ) 
      {
        val functionStartIdx = insPtrs(i)
        
      }
      result
    }
    
    def getEntries : ( Seq[VariablesTable] , Seq[Int] , Seq[JumpTableEntry] ) = 
    {
      ( stackFrames ,  insPtrs.toSeq.sortBy( _._1 ).map( _._2 )  , labelToSlot.values.toSeq )
    }
    
    def declareFunction(signature : FunctionSignature,scope:VariablesTable): Unit = maybeCreateFunctionEntry(signature.name,Some(signature),-1 , scope ) // -1 is special marker for unresolved (external) function 
    
    def defineFunction(signature : FunctionSignature,scope:VariablesTable): Unit = maybeCreateFunctionEntry(signature.name,Some(signature),insBuffer.size , scope )
    
    private[this] def maybeCreateFunctionEntry(name:Identifier,signature : Option[FunctionSignature],insOffset:Int,scope:VariablesTable) : Unit = 
    {
      val entry = labelToSlot.get( name ) match 
      {
        case Some(x) => x
        case None => 
        {
           val idx = jmpTableIndex
           jmpTableIndex += 1
           val newEntry = new JumpTableEntry(name,signature,idx)
           labelToSlot += name -> newEntry
           stackFrames += scope
           newEntry
        }
      }
      insPtrs += entry.slotIndex -> insOffset
    }
    
    def addInsn(op:Opcode) : Unit = 
    {
      println("INSN "+insBuffer.size+" : "+op)
      insBuffer += op
    }     
     
    def slotIndex(symbol:Symbol) : Int = labelToSlot(symbol.name).slotIndex 
           
    def contains(symbol:Symbol) : Boolean = labelToSlot.contains( symbol.name )
  }
  
  final class VariablesTable(val scope : Scope) 
  {
     var slotIndex = 0
     val varSlots = new HashMap[Identifier,VarEntry]()
     
     def frameSize : Int = varSlots.size
     
     def sortedEntries : Seq[VarEntry] = varSlots.values.toSeq.sortBy( _.slotNum )
     
     def registerVariable(name:Identifier,kind:TypeName) : Int  = 
     {
       varSlots.get(name) match 
       {
         case Some(entry) => entry.slotNum
         case None => 
         {
             println("Registering variable "+name+" with kind "+kind+" in frame "+scope.fullyQualifiedName)
             val idx = slotIndex
             varSlots += name -> VarEntry(name,kind,slotIndex)
             slotIndex+=1
             idx
         }
       }
     }   
     
     def varEntries : Seq[VarEntry] = varSlots.values.toSeq
     
     def hasParent : Boolean = scope.hasParent
     
     def getVariableSlot(name:Identifier,kind:TypeName) : Option[Int] = 
     {
       val existing : Option[VarEntry] = varSlots.get(name)
       existing match 
       {
         case Some(entry) if kind.canBeAssignedTo( entry.kind ) => Some(entry.slotNum) 
         case Some(entry) => throw new RuntimeException( kind+" cannot be assigned to "+entry)
         case None => None
       }
     }
  }
  
  private final class DefaultContext extends ICompilationContext 
  {
     val jumpTable = new JumpTable()
     val constantPool = new ConstantPool
     val stackFrames = HashMap[String,VariablesTable]() 
     
     private[this] var currentFunction : Option[Identifier] = None
     
     private[this] val stack = Stack[VariablesTable]()
     
     private[this] var passNo = 0 
          
     override def currentPassNo : Int = passNo
     
     def incPassNo() = passNo += 1
     
     def declareFunction(signature : FunctionSignature): Unit = jumpTable.declareFunction( signature , currentFrame )
     
     def defineFunction(signature : FunctionSignature): Unit = jumpTable.defineFunction( signature , currentFrame )
     
     private[this] def currentFrame : VariablesTable = stack.top
     
     def slotOf(symbol:Symbol,scope:Scope) : Int = 
     {
       symbol.symbolType match 
       {
         case SymbolType.FUNCTION_NAME =>  jumpTable.slotIndex(symbol)
         case SymbolType.LABEL => jumpTable.slotIndex(symbol)
         case SymbolType.VARIABLE => 
         {           
           val sym = symbol.asInstanceOf[ValueSymbol]
           var frame : Option[VariablesTable] = Some( frameForScope( scope ) )
           do {
             val found = frame.get.getVariableSlot( symbol.name , sym.node.evaluate().kind )
             if ( found.isDefined ) {
               return found.get
             }
             frame = if ( frame.get.hasParent ) parentFrame(frame.get) else None
           } while ( frame.isDefined )
           throw new RuntimeException("Failed to find variable "+symbol+" in any frame for scope "+scope)          
         }
         case _ => throw new RuntimeException("Neither a function name nor a label: "+symbol)
       }
     }
     
     private[this] def parentFrame(frame:VariablesTable) : Option[VariablesTable] =  if ( frame.hasParent ) Some( frameForScope( frame.scope.parent.get ) ) else None
     
     override def registerConstant(symbol:ValueSymbol) : Unit = 
     {
         val value = symbol.node.evaluate()
         value match 
         {
           case v @ TypedValue(Some(_),_) => constantPool.slotOf( v )
           case _ => throw new RuntimeException("Need to have a value for "+symbol)
         }
     }
     
    def topLevelFrame : VariablesTable = stackFrames( Scope.GLOBAL_SCOPE_NAME )
     
    override def newStackFrame(scope: Scope): Unit =  stack.push( frameForScope( scope ) )
    
    private[this] def frameForScope(scope:Scope) : VariablesTable =  stackFrames.getOrElseUpdate( scope.fullyQualifiedName ,  new VariablesTable(scope) )
    
    override def currentScope: Scope = stack.top.scope

    override def popStackFrame(): Unit = stack.pop    
     
    override def registerVariable(name: Identifier, kind: TypeName): Unit = 
    {
      currentFrame.registerVariable( name , kind )
    }
    
    def beginFunction(signature:FunctionSignature,scope:Scope) : Unit = 
    {
      currentFunction = Some(signature.name)
      jumpTable.defineFunction( signature , frameForScope( scope ) )
      newStackFrame( scope )
    }
     
     def endFunction() : Unit = 
     {
       currentFunction = None
       popStackFrame()       
     }
     
    // ============ EMIT =======
    
   override def emitPop() = addInsn( Opcode.POP ) // pop value from stack
   
   override def emitLoad(symbol: Symbol): Unit = 
   {
     val scope = currentScope.getOwningScope( symbol ).get 
     val frame = frameForScope( scope )
     symbol match 
     {
       case x : ValueSymbol => 
       {
         if ( scope.isGlobalScope ) {
        	 addInsn( Opcode.LOAD_HEAP( slotOf( x , currentScope ) ) ) // push value on stack
         } else {
           addInsn( Opcode.LOAD_STACK( slotOf( x , currentScope ) ) )
         }
       }
       case _ => throw new RuntimeException("Unreachable code reached")
     }
   }
     
    override def emitStore(symbol:ValueSymbol) = 
    { 
      val scope = currentScope.getOwningScope( symbol ).get 
      val frame = frameForScope( scope )
      if ( scope.isGlobalScope ) 
      {
    	  addInsn( Opcode.STORE_HEAP( slotOf( symbol , currentScope ) ) ) // pop value from stack and store it at given location
      } else {
        addInsn( Opcode.STORE_STACK( slotOf( symbol , currentScope ) ) )
      }
    }
     
    override def emitJumpSubroutine(symbol:LabelSymbol) = addInsn( Opcode.JSR( slotOf( symbol , topLevelFrame.scope ) ) ) // pop value from stack and jump to this location
     
    override def emitAdd() = addInsn( Opcode.ADD ) // pop two values from stack , add them and push result
     
    override def emitSub() = addInsn( Opcode.SUB ) // pop two values from stack , subtract them and push result     

    private[this] def addInsn(op:Opcode) 
    {
       jumpTable.addInsn( op )
    }

    override def emitLoad(value: TypedValue ): Unit = 
    {
      val slot = constantPool.slotOf(value)
      addInsn( Opcode.LOAD_CONST( slot ) )
    }

    override def emitReturn(): Unit = addInsn( Opcode.RETURN )
  }
  
  private[this] def filter[T <: IASTNode]( clazz : Class[T],nodes: Iterable[IASTNode]) : Iterable[T] = {
    nodes.filter( _.getClass == clazz ).map( _.asInstanceOf[ T ] )
  }
  
  def compile( ast : AST) : BinaryFile =
  {
    validate( ast )
     
    val ctx = new DefaultContext
    ast.visit( ctx )
    
    println("Compilation yielded "+ctx.stackFrames.size+" frames")
   
    // print constant pool entries
    printConstantPool( ctx.constantPool.values )
    // print jump table
    printJumpTable( ctx.jumpTable.getEntries._3 , ctx.jumpTable.insPtrs.toSeq.sortBy( _._1 ).map( _._2 ) )
    // print stack frames
    ctx.jumpTable.getEntries._1.foreach( table => printFrame(table.sortedEntries , table.slotIndex ) ) 
    
    val result = new BinaryFile()
    
    val jmp = ctx.jumpTable.getEntries 
    result.setJumpTable( jmp._1 , jmp._2 , jmp._3 , ctx.topLevelFrame )
    result.setConstantPool( ctx.constantPool.values )
    result.setInstructions( ctx.jumpTable.insBuffer )
    
    val test = new ByteArrayOutputStream
    result.writeTo( test )
    
    val result2 = BinaryFile.readFrom( new ByteArrayInputStream( test.toByteArray() ) )
    
    println("====================")
    println("==== Read file =====")
    println("====================")

    printConstantPool( result2.getConstantPool() )
    val jumpTable = result2.getJumpTable()
    // print jump table
    printJumpTable( jumpTable._3 , jumpTable._2 )
    // print stack frames
    var stackFrames = jumpTable._1
    for ( idx <- 0 until stackFrames.size ) {
       printFrame( stackFrames(idx) , idx )
    }
    
    println("Output file has "+test.toByteArray().length+" bytes")
    result
  }
  
  private[this] def printConstantPool( vars : Seq[ConstantEntry] ) : Unit = {
    println("=== Constant pool ===")
    
    for ( idx <- 0 until vars.size  ) 
    {
      val value = vars(idx)
      println( "Index "+idx+": <ANON> = "+value)
    }    
  }
  
  private[this] def printJumpTable(entries:Seq[JumpTableEntry],insPtrs : Seq[Int] ) 
  {
    entries.sortBy( _.slotIndex ).foreach( entry => println( "Jumptable slot #"+entry.slotIndex+": "+entry.signature+" starts at instruction "+insPtrs(entry.slotIndex)) )
  }   
  
  private[this] def printFrame(frame:Seq[ VarEntry ] , frameIndex : Int ) 
  {
    println("=== Frame #"+frameIndex+" ===")
    
    // print stack layout
    for ( idx <- 0 until frame.size) 
    {
      println( "Slot #"+idx+": variable "+ frame(idx).name )
    }
  }
  
  private[this] def findMainMethod(frame:VariablesTable) : Option[FunctionDefinition] = 
  {
    frame.scope.getSymbol( Compiler.MAIN_METHOD_IDENTIFIER.name , SymbolType.FUNCTION_NAME ) match 
    {
      case Some(symbol) if Compiler.isMainMethod(symbol) => Some( symbol.asInstanceOf[LabelSymbol].node.asInstanceOf[FunctionDefinition] ) 
      case _ => None
    }
  } 
  
  private[this] def validate(ast:AST) 
  {
   for ( funcNode <- filter( classOf[ FunctionDefinition ] , ast) ;
         paramList <- filter( classOf[ ParameterList ] , funcNode) ; 
         arg <- filter( classOf[ FunctionArgument] , paramList) )
   {
     if ( ! arg.kind.isKnownType ) {
       throw new RuntimeException("Unsupported type: "+arg.kind)
     }
   }
  }
}

object Compiler 
{
  val MAIN_METHOD_IDENTIFIER = new FunctionSignature(Identifier("main"),KnownTypes.UNIT)
  val INIT_METHOD_IDENTIFIER = new FunctionSignature(Identifier("__init__"),KnownTypes.UNIT)
  
  def isMainMethod(node:IASTNode) : Boolean = 
  {
    if ( node.isInstanceOf[FunctionDefinition] ) 
    {
      val func = node.asInstanceOf[FunctionDefinition]
      if  ( func.returnType != KnownTypes.UNIT ) {
        println( func+" node matched, wrong return type "+func.returnType)
        return false
      }
      if ( func.parameterListCount != 1 ) {
        println( func+" node matched, bad parameter list count "+func.parameterListCount)
        return false
      }
      if ( ! func.parameterList(0).hasNoParameters ) {
        println( func+" node matched, parameter list is not empty")
        return false
      }
      true
    } else {
      false
    }
  }
  
  protected def isMainMethod(symbol:Symbol) : Boolean =  
  {
    if ( symbol.symbolType == SymbolType.FUNCTION_NAME && symbol.name == MAIN_METHOD_IDENTIFIER.name ) 
    {
      val n = symbol.asInstanceOf[LabelSymbol].node.asInstanceOf[FunctionDefinition]
      isMainMethod(n)
    } else {
      false
    }
  }  
}

sealed case class ConstantEntry(value:Any,kind:TypeName)