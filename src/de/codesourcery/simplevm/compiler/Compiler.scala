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

class Compiler 
{
  private[this] def filter[T <: IASTNode]( clazz : Class[T],nodes: Iterable[IASTNode]) : Iterable[T] = {
    nodes.filter( _.getClass == clazz ).map( _.asInstanceOf[ T ] )
  }
  
  def compile( ast : AST) 
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
     
    ctx.stackFrames.values.foreach( printFrame )  
  }
  
  private[this] def printFrame(frame:MyFrame) 
  {
    println("=== Frame: "+frame.scope.name+" ===")
    
    // print stack layout
    for ( idx <- 0 until frame.slotIndex ) 
    {
      val key = frame.varSlots.filter{ case (k,v) => v == idx }.map( pair => pair._1 ).head
      println( "Slot #"+idx+": variable "+key)
    }
    
    // print jump table
    frame.jumpTable.foreach( _ match { case (k,v) => println( "Jumptable #"+k+": "+v) } )
    
    // print instructions
    var idx = 0
    for ( ins <- frame.insBuffer ) 
    {
      println("Instruction "+idx+": "+ins)
      idx += 1
    }
  }
  
  private[this] def findMainMethod(frame:MyFrame) : Option[FunctionDefinition] = 
  {
    frame.scope.getSymbol( Identifier("main") , SymbolType.FUNCTION_NAME ) match 
    {
      case Some(symbol) => 
      {  
        val n = symbol.asInstanceOf[LabelSymbol].node.asInstanceOf[FunctionDefinition] 
        if ( n.parameterListCount == 1 && n.parameterList(0).hasNoParameters && n.returnType == KnownTypes.UNIT ) 
        {
          Some(n)
        } else {
          None
        }
      }
      case None => None
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
  
  private final class ConstantPool 
  {
    val values = new ListBuffer[Any]()
    
    def slotOf(value:Any) : Int = 
    {
       var index = 0;
       while ( index < values.size ) 
       {
         if ( values(index) == value ) {
               return index
         }
         index += 1
       }
       val result = values.size
       value match 
       {
         case Some(x) => values += x
         case _ => values += value
       }
       result
    }
  }
  
  private sealed case class VarEntry(name:Identifier,kind:TypeName)
  
  private final class MyFrame(val scope : Scope) 
  {
     var slotIndex = 0
     val insBuffer = new ListBuffer[Opcode]()
     val varSlots = new HashMap[VarEntry,Int]()
     val jumpTable = new HashMap[Identifier,Int]()
     
     def addInsn(op:Opcode) : Unit = 
     {
       insBuffer += op
       println("INSN: "+op)
     }
     
     def registerFunction(name:Identifier) : Unit = jumpTable += name -> insBuffer.size
     
     def slotOf(symbol:Symbol) : Int = 
     {
       symbol.symbolType match 
       {
         case SymbolType.FUNCTION_NAME => jumpTable(symbol.name) 
         case SymbolType.LABEL => jumpTable(symbol.name)
         case SymbolType.VARIABLE => 
         {
           
           val sym = symbol.asInstanceOf[ValueSymbol]
           val key = VarEntry( symbol.name , sym.node.evaluate().kind )
           varSlots(key)
         }
         case _ => throw new RuntimeException("Neither a function name nor a label: "+symbol)
       }
     }
     
     def registerVariable(name:Identifier,kind:TypeName) : Int  = 
     {
      val key = VarEntry( name , kind )       
       varSlots.get(key) match 
       {
         case Some(index) => index
         case None => 
         {
             val idx = slotIndex
             varSlots += key -> slotIndex
             slotIndex+=1
             idx
         }
       }
     }          
  }
  
  private final class DefaultContext extends ICompilationContext 
  {
     val constantPool = new ConstantPool
     val stackFrames = HashMap[String,MyFrame]() 
     
     private[this] val stack = Stack[MyFrame]()
     
     private[this] var passNo = 0 
          
     override def currentPassNo : Int = passNo
     
     def incPassNo() = passNo += 1
     
     private[this] def currentFrame : MyFrame = stack.top
     
     override def registerConstant(symbol:ValueSymbol) : Unit = 
     {
         val value = symbol.node.evaluate()
         value match 
         {
           case TypedValue(Some(x),_) => constantPool.slotOf( x )
           case _ => throw new RuntimeException("Need to have a value for "+symbol)
         }
     }
     
    override def pushScope(scope: Scope): Unit =  stack.push( frameForScope( scope ) )
    
    private[this] def frameForScope(scope:Scope) : MyFrame =  stackFrames.getOrElseUpdate( scope.fullyQualifiedName ,  new MyFrame(scope) )
    
    override def currentScope: Scope = stack.top.scope

    override def popScope(): Unit = stack.pop    
     
    override def registerVariable(name: Identifier, kind: TypeName): Unit = currentFrame.registerVariable( name , kind )
    
    override def registerFunction(name:Identifier) : Unit = currentFrame.registerFunction( name )
    // ============ EMIT =======
    
   override def emitPop() = addInsn( Opcode.POP ) // pop value from stack
   
   override def emitLoad(symbol: Symbol): Unit = 
   {
     val frame = frameForScope( currentScope.getOwningScope( symbol ).get )
     symbol match 
     {
       case x : ValueSymbol => addInsn( Opcode.LOAD_VARIABLE( frame.slotOf( x ) ) ) // push value on stack
       case x : LabelSymbol => addInsn( Opcode.LOAD_ADDRESS( frame.slotOf( x ) ) )
       case _ => throw new RuntimeException("Unreachable code reached")
     }
   }
     
    override def emitStore(symbol:ValueSymbol) = 
    { 
      val frame = frameForScope( currentScope.getOwningScope( symbol ).get )      
      addInsn( Opcode.store( frame.slotOf( symbol ) ) ) // pop value from stack and store it at given location
    }
     
    override def emitJump() = addInsn( Opcode.JUMP ) // pop value from stack and jump to this location
     
    override def emitAdd() = addInsn( Opcode.ADD ) // pop two values from stack , add them and push result
     
    override def emitSub() = addInsn( Opcode.SUB ) // pop two values from stack , subtract them and push result     

    private[this] def addInsn(op:Opcode) 
    {
       currentFrame.addInsn( op )
    }

    override def emitLoad(value: Any): Unit = 
    {
      val slot = constantPool.slotOf(value)
      addInsn( Opcode.LOAD_CONST( slot ) )
    }

    override def emitReturn(): Unit = addInsn( Opcode.RETURN )
  }
}