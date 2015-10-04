package de.codesourcery.simplevm.parser.ast

import de.codesourcery.simplevm.parser.TypeName

sealed case class TypedResult(val value:Option[Any],val kind:TypeName)
