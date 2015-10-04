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
import de.codesourcery.simplevm.parser.ast.TypedResult
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
     
     println("Compilation yielded "+ctx.allFrames.size+" frames")
     ctx.allFrames.foreach( printFrame )
    // val globalScope = ctx.allFrames.filter( frame => frame.scope.isGlobalScope ).head     
  }
  
  private[this] def printFrame(frame:MyFrame) 
  {
    println("=== Frame: "+frame.scope.name+" ===")
    // print constant pool entries
    for ( idx <- 0 until frame.constantPool.length ) 
    {
      val value = frame.constantPool(idx)
      val key = frame.indices.filter{ case (k,v) => v == idx }.map( pair => pair._1 ).headOption
      if ( key.isEmpty ) {
        println( "Index "+idx+": <ANON> = "+value)
      } else {
        println( "Index "+idx+": "+key.get+" = "+value)
      }
    }
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
  
  private final class MyFrame(val scope : Scope) 
  {
     val insBuffer = new ListBuffer[Opcode]()
     val indices = new HashMap[Identifier,Int]()
     val constantPool = new ListBuffer[Any]()
     
     def addInsn(op:Opcode) : Unit = 
     {
       insBuffer += op
       println("INSN: "+op)
     }
     
     def storeConstant(name:Identifier, v : TypedResult )  = 
     {
       val idx = registerVariable(name , v.kind )
       constantPool( idx ) = v.value
     }
     
     def slotIndexOf(symbol:Symbol) : Int = {
       val result = indices.get( symbol.name )
       if ( ! result.isDefined ) 
       {
          throw new RuntimeException("Internal error,failed to find slot for symbol "+symbol+" in frame "+scope)   
       }
       result.get
     }
     
     def registerFunction(name:Identifier) : Unit = 
     {
       println("Registering function "+name+" , insBuffser size: "+insBuffer.size)
       indices.get(name) match 
       {
         case Some(index:Int) => constantPool(index) = insBuffer.size
         case None => 
         {
             val idx = constantPool.size
             constantPool += insBuffer.size
             indices += name -> idx
         }
       }       
     }
     
     def registerVariable(name:Identifier,kind:TypeName) : Int  = 
     {
       indices.get(name) match 
       {
         case Some(index) => index
         case None => 
         {
             val idx = constantPool.size
             constantPool += null
             indices += name -> idx
             idx
         }
       }
     }     
     
     def getOrStoreConstant(value:Any) : Int = 
     {
       constantPool.indexOf( value ) match {
         case -1 => 
         {
           val idx = constantPool.size  
           if ( value.getClass == classOf[ Some[_] ] ) 
           {
             constantPool += value.asInstanceOf[Some[_]].get
           } else {
             constantPool += value
           }
           idx
         }
         case x => x
       }
     }
  }
  
  private final class DefaultContext extends ICompilationContext 
  {
     val allFrames = ListBuffer[MyFrame]() 
     
     private[this] val stack = Stack[MyFrame]()
     
     private[this] var passNo = 0 
          
     override def currentPassNo : Int = passNo
     
     def incPassNo() = passNo += 1
     
     private[this] def currentFrame : MyFrame = stack.top
     
     override def registerConstant(symbol:ValueSymbol) : Unit = 
     {
         currentFrame.storeConstant( symbol.name , symbol.node.evaluate() )
     }
     
    override def pushScope(scope: Scope): Unit = stack.push( frameForScope( scope ) )
    
    private[this] def frameForScope(scope:Scope) : MyFrame = 
    {
      allFrames.filter( frame => frame.scope == scope ).headOption match 
      {
        case Some(frame) => frame
        case None => {
          val frame = new MyFrame(scope)
          allFrames += frame
          frame
        }
      }
    }
    
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
       case x : ValueSymbol => addInsn( Opcode.LOAD_VARIABLE( frame.slotIndexOf( x ) ) ) // push value on stack
       case x : LabelSymbol => addInsn( Opcode.LOAD_ADDRESS( frame.slotIndexOf( x ) ) )
       case _ => throw new RuntimeException("Unreachable code reached")
     }
   }
     
    override def emitStore(symbol:ValueSymbol) = 
    { 
      val frame = frameForScope( currentScope.getOwningScope( symbol ).get )      
      addInsn( Opcode.store( frame.slotIndexOf( symbol ) ) ) // pop value from stack and store it at given location
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
      val slot = currentFrame.getOrStoreConstant(value)
      addInsn( Opcode.LOAD_CONST( slot ) )
    }

    override def emitReturn(): Unit = addInsn( Opcode.RETURN )
  }
}