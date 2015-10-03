package de.codesourcery.simplevm.parser

import javax.swing.JFrame
import javax.swing.tree.TreeModel
import de.codesourcery.simplevm.parser.ast.IASTNode
import javax.swing.tree.TreePath
import scala.collection.mutable.ArrayBuffer
import javax.swing.event.TreeModelListener
import javax.swing.JTree
import javax.swing.JScrollPane
import java.awt.Dimension
import javax.swing.tree.DefaultTreeCellRenderer
import java.awt.Component

object ParserTest {
  
  def main(args:Array[String]) 
  {
    val code = """
         def func1(a:int8,b:int8)(c:string,d:int8) 
         {
             func2 { 3+4,5 }
         }
    """
    var lexer = new Lexer(new StringScanner(code) )
    while ( ! lexer.eof ) {
      println("GOT: "+lexer.next())
    }
    lexer = new Lexer(new StringScanner(code))
    val parser = new Parser(lexer)
    val ast = parser.parse()

    println( ast.print(0) )
    
    val frame = new JFrame()
    
    val treeModel = new TreeModel() 
    {
        private[this] val listeners = new ArrayBuffer[TreeModelListener]
      
        def getRoot() = ast

        def getChild(parent:AnyRef, index:Int) : AnyRef = parent.asInstanceOf[IASTNode].child(index)

        def getChildCount(parent:AnyRef) : Int = parent.asInstanceOf[IASTNode].childCount

        def isLeaf(node:AnyRef) : Boolean = node.asInstanceOf[IASTNode].childCount == 0

        def valueForPathChanged(path:TreePath, newValue: AnyRef ) : Unit = {}

        def getIndexOfChild(parent:AnyRef, child:AnyRef) : Int = parent.asInstanceOf[IASTNode].children.indexOf( child.asInstanceOf[IASTNode] )

        def addTreeModelListener( l : TreeModelListener) : Unit = listeners += l

        def removeTreeModelListener( l : TreeModelListener) : Unit = listeners -= l
    }
    
    val tree = new JTree( treeModel )
    
    tree.setCellRenderer( new DefaultTreeCellRenderer() 
    {
      override def getTreeCellRendererComponent(tree:JTree, value : AnyRef,sel:Boolean,expanded:Boolean,leaf:Boolean,row:Int,hasFocus:Boolean) : Component = 
      {
        val result = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
        if ( value.isInstanceOf[IASTNode] ) {
          setText( "[" +value.getClass.getSimpleName + "] "+ value.asInstanceOf[IASTNode].print(0) )
        }
        result
      }
    })
    
    
    frame.getContentPane.add( new JScrollPane( tree ) )
    frame.setPreferredSize( new Dimension(640,480) )
    frame.pack()
    frame.setVisible(true)
  }
}