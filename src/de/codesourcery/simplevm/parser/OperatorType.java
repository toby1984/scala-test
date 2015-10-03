package de.codesourcery.simplevm.parser;

import java.util.Arrays;

public enum OperatorType 
{
    PLUS("+"),
    MINUS("-"),
    ASSIGNMENT("="),
    FUNCTION_CALL("") 
    {
        @Override
        public boolean matchedByLexer() {
            return false;
        }
    };
    
    public final String symbol;
    
    private OperatorType(String symbol) {
        this.symbol = symbol;
    }
    
    public boolean matchedByLexer() {
        return true;
    }
    
    public boolean isInfixOperator() {
        return true;
    }
    
    public static boolean isValidOperator(String s) 
    {
        return Arrays.stream( values() ).filter( OperatorType::matchedByLexer ).anyMatch( type -> type.symbol.equals( s ) );
    }
    
    public static OperatorType getOperatorType(String s) 
    {
        OperatorType result = null;
        for ( OperatorType t : values() ) 
        {
            if ( t.symbol.equals( s ) ) {
                if (result != null ) {
                    throw new IllegalArgumentException("Ambiguous operator symbol: '"+s+"'");
                }
                result = t;
            }
        }
        if ( result == null ) {
            throw new IllegalArgumentException("Unknown operator symbol: '"+s+"'");
        }
        return result;
    }    
}
