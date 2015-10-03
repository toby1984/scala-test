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
import de.codesourcery.simplevm.compiler.Compiler
import javax.swing.ToolTipManager
import de.codesourcery.simplevm.parser.ast.AST
import javax.swing.event.TreeModelEvent
import javax.swing.JPanel
import javax.swing.JTextField
import java.awt.BorderLayout
import javax.swing.JTextArea
import javax.swing.event.DocumentListener
import javax.swing.event.DocumentEvent
import javax.swing.JButton

object ParserTest 
{
  private[this] val treeModel = new MyTreeModel
  
  class MyTreeModel extends TreeModel 
  {
      private[this] var ast : AST = new AST()
      
      private[this] val listeners = new ArrayBuffer[TreeModelListener]
    
      override def getRoot() = ast

      override def getChild(parent:AnyRef, index:Int): AnyRef = parent.asInstanceOf[IASTNode].child(index)

      override def getChildCount(parent:AnyRef): Int = parent.asInstanceOf[IASTNode].childCount

      override def isLeaf(node:AnyRef): Boolean = node.asInstanceOf[IASTNode].childCount == 0

      override def valueForPathChanged(path:TreePath, newValue: AnyRef ): Unit = {}

      override def getIndexOfChild(parent:AnyRef, child:AnyRef): Int = parent.asInstanceOf[IASTNode].children.indexOf( child.asInstanceOf[IASTNode] )

      override def addTreeModelListener( l : TreeModelListener): Unit = listeners += l

      override def removeTreeModelListener( l : TreeModelListener): Unit = listeners -= l
      
      def setModelObject(ast:AST) 
      {
        this.ast = ast
        val ev = new TreeModelEvent(this,new TreePath(ast) )
        listeners.foreach { l => l.treeStructureChanged( ev ) }
      }
  }
  
  private[this] def toTooltipText(scope : Scope ) : String = 
  {
    val value = scope.getAllSymbols.map( { case(key,value) => "<B>"+key+"</B>="+value } ).mkString("<BR />")
    "<HTML>"+value+"</HTML>"  
  }
  
  private[this] def parse(code:String) 
  {
   var lexer = new Lexer(new StringScanner(code) )
    while ( ! lexer.eof ) {
      println("GOT: "+lexer.next())
    }
    lexer = new Lexer(new StringScanner(code))
    val parser = new Parser(lexer)
    val ast = parser.parse()
    treeModel.setModelObject( ast )

    println( ast.print(0) )
  }
  
  def main(args:Array[String]) 
  {
    val code = """val global = 42
         def func1(a:int8,b:blubb)(c:string[] ,d:int8[] => string) 
         {
             func2 { 3+4,5 }
             val e = "test"
         }
    """
    parse( code )
    
    val frame = new JFrame()
    frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE )
    
    val tree = new JTree( treeModel )
    
    ToolTipManager.sharedInstance().registerComponent( tree )
    
    tree.setCellRenderer( new DefaultTreeCellRenderer() 
    {
      override def getTreeCellRendererComponent(tree:JTree, value : AnyRef,sel:Boolean,expanded:Boolean,leaf:Boolean,row:Int,hasFocus:Boolean) : Component = 
      {
        val result = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus)
        if ( value.isInstanceOf[IASTNode] ) {
          setText( "[" +value.getClass.getSimpleName + "] "+ value.asInstanceOf[IASTNode].print(0) )
          setToolTipText( toTooltipText( value.asInstanceOf[IASTNode].scope.get ) )
        }
        result
      }
    })
    
    val panel = new JPanel
    panel.setLayout( new BorderLayout )
    
    val pane = new JScrollPane( tree )
    pane.setMinimumSize( new Dimension(640,480) )
    
    val textfield = new JTextArea
    textfield.setText( code )
    textfield.setMinimumSize( new Dimension(640,100) )
    
    val button = new JButton("Compile")
    button.addActionListener( ev => parse( textfield.getText ) )
    
    panel.add( pane , BorderLayout.NORTH )
    panel.add( button, BorderLayout.WEST )
    panel.add(new JScrollPane( textfield ) , BorderLayout.CENTER )
    
    frame.getContentPane.add( panel )
    frame.setPreferredSize( new Dimension(640,480) )
    frame.pack()
    frame.setVisible(true)
  }
}