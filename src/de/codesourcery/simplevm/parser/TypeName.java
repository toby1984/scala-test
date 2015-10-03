package de.codesourcery.simplevm.parser;

import java.util.Arrays;

public enum TypeName 
{
   INT8("int8"),
   STRING("string");
    
   public final String symbol;
    
   private TypeName(String symbol) {
     this.symbol = symbol;
   }
   
   public static boolean isValidType(String s) 
   {
       return Arrays.stream( values() ).anyMatch( x -> x.symbol.equals( s ) );
   }
   
   public static TypeName fromString(String s) {
       return Arrays.stream( values() ).filter( x -> x.symbol.equals( s ) ).findFirst().orElseThrow( () -> new IllegalArgumentException("Unknown type '"+s+"'" ) );
   }
}
