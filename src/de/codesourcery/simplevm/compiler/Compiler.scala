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
    val functionToSlot = new HashMap[Identifier,Int]()
    
    def toSeq() : Seq[Int] = 
    {
      insPtrs.toSeq.sortBy( pair => pair._1 ).map( pair => pair._2)
    }
    
    def registerFunction(name:Identifier) : Int = 
    {
      functionToSlot.get(name) match 
      {
        case Some(idx) => idx
        case None => 
        {
          val idx = jmpTableIndex
          functionToSlot += name -> idx
          insPtrs += idx -> insBuffer.size
          jmpTableIndex += 1 
          idx
        }
      }
    }               
    
    def addInsn(op:Opcode) : Unit = 
    {
      println("INSN "+insBuffer.size+" : "+op)
      insBuffer += op
    }     
     
    protected[Compiler] def printJumpTable() 
    {
      // print jump table
      functionToSlot.foreach( _ match { case (k,slot) => println( "Jumptable: "+k+"(...) in slot # "+slot+" starts at instruction "+insPtrs(slot)) } )
      
      // print instructions
      var idx = 0
      for ( ins <- functionToSlot ) 
      {
        println("Instruction "+idx+": "+ins)
        idx += 1
      }    
    }     
    
    def slotIndex(symbol:Symbol) : Int = functionToSlot(symbol.name) 
           
    def contains(symbol:Symbol) : Boolean = functionToSlot.contains( symbol.name )
  }
  
  case class VarEntry(val name:Identifier,val kind:TypeName,val slotNum:Int)  
  
  private final class MyFrame(val scope : Scope) 
  {
     var slotIndex = 0
     val varSlots = new HashMap[Identifier,VarEntry]()
     
     def frameSize : Int = slotIndex
     
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
     val stackFrames = HashMap[String,MyFrame]() 
     
     private[this] var currentFunction : Option[Identifier] = None
     
     private[this] val stack = Stack[MyFrame]()
     
     private[this] var passNo = 0 
          
     override def currentPassNo : Int = passNo
     
     def incPassNo() = passNo += 1
     
     def registerFunction(name:Identifier) : Unit = jumpTable.registerFunction( name )
     
     private[this] def currentFrame : MyFrame = stack.top
     
     def slotOf(symbol:Symbol,scope:Scope) : Int = 
     {
       symbol.symbolType match 
       {
         case SymbolType.FUNCTION_NAME =>  jumpTable.slotIndex(symbol)
         case SymbolType.LABEL => jumpTable.slotIndex(symbol)
         case SymbolType.VARIABLE => 
         {           
           val sym = symbol.asInstanceOf[ValueSymbol]
           var frame : Option[MyFrame] = Some( frameForScope( scope ) )
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
     
     private[this] def parentFrame(frame:MyFrame) : Option[MyFrame] =  if ( frame.hasParent ) Some( frameForScope( frame.scope.parent.get ) ) else None
     
     override def registerConstant(symbol:ValueSymbol) : Unit = 
     {
         val value = symbol.node.evaluate()
         value match 
         {
           case v @ TypedValue(Some(_),_) => constantPool.slotOf( v )
           case _ => throw new RuntimeException("Need to have a value for "+symbol)
         }
     }
     
    private[this] def topLevelFrame : MyFrame = stackFrames( Scope.GLOBAL_SCOPE_NAME )
     
    override def pushScope(scope: Scope): Unit =  stack.push( frameForScope( scope ) )
    
    private[this] def frameForScope(scope:Scope) : MyFrame =  stackFrames.getOrElseUpdate( scope.fullyQualifiedName ,  new MyFrame(scope) )
    
    override def currentScope: Scope = stack.top.scope

    override def popScope(): Unit = stack.pop    
     
    override def registerVariable(name: Identifier, kind: TypeName): Unit = currentFrame.registerVariable( name , kind )
    
     def beginFunction(name:Identifier,scope:Scope) : Unit = 
     {
      currentFunction = Some(name)
      jumpTable.registerFunction( name )
      pushScope( scope )
    }
     
     def endFunction() : Unit = 
     {
       jumpTable.registerFunction( currentFunction.get )
       currentFunction = None
       
       popScope()       
     }
     
    // ============ EMIT =======
    
   override def emitPop() = addInsn( Opcode.POP ) // pop value from stack
   
   override def emitLoad(symbol: Symbol): Unit = 
   {
     val frame = frameForScope( currentScope.getOwningScope( symbol ).get )
     symbol match 
     {
       case x : ValueSymbol => addInsn( Opcode.LOAD_VARIABLE( slotOf( x , currentScope ) ) ) // push value on stack
       case x : LabelSymbol => addInsn( Opcode.LOAD_ADDRESS( slotOf( x , currentScope ) ) )
       case _ => throw new RuntimeException("Unreachable code reached")
     }
   }
     
    override def emitStore(symbol:ValueSymbol) = 
    { 
      val frame = frameForScope( currentScope.getOwningScope( symbol ).get )      
      addInsn( Opcode.store( slotOf( symbol , currentScope ) ) ) // pop value from stack and store it at given location
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
    println("=== Constant pool ===")
    
    val pool = ctx.constantPool
    for ( idx <- 0 until pool.values.size  ) 
    {
      val value = pool.values(idx)
      println( "Index "+idx+": <ANON> = "+value)
    }
    
    ctx.jumpTable.printJumpTable()
    
    ctx.stackFrames.values.foreach( printFrame )  
    
    val result = new BinaryFile()
    result.setJumpTable( ctx.jumpTable.toSeq )
    result.setConstantPool( ctx.constantPool.values )
    result.setInstructions( ctx.jumpTable.insBuffer )
    
    val test = new ByteArrayOutputStream
    result.writeTo( test )
    println("Output file has "+test.toByteArray().length+" bytes")
    result
  }
  
  private[this] def printFrame(frame:MyFrame) 
  {
    println("=== Frame: "+frame.scope.name+" ===")
    
    // print stack layout
    for ( idx <- 0 until frame.slotIndex ) 
    {
      val key = frame.varSlots.filter{ case (k,v) => v.slotNum == idx }.map( pair => pair._1 ).head
      println( "Slot #"+idx+": variable "+key)
    }
  }
  
  private[this] def findMainMethod(frame:MyFrame) : Option[FunctionDefinition] = 
  {
    frame.scope.getSymbol( Compiler.MAIN_METHOD_IDENTIFIER , SymbolType.FUNCTION_NAME ) match 
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
  val MAIN_METHOD_IDENTIFIER = Identifier("main")
  
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
    if ( symbol.symbolType == SymbolType.FUNCTION_NAME && symbol.name == MAIN_METHOD_IDENTIFIER ) 
    {
      val n = symbol.asInstanceOf[LabelSymbol].node.asInstanceOf[FunctionDefinition]
      isMainMethod(n)
    } else {
      false
    }
  }  
}

sealed case class ConstantEntry(value:Any,kind:TypeName)