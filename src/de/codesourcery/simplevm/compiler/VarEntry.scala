package de.codesourcery.simplevm.compiler

import de.codesourcery.simplevm.parser.Identifier
import de.codesourcery.simplevm.parser.TypeName

sealed case class VarEntry(val name:Identifier,val kind:TypeName,val slotNum:Int)  