package de.codesourcery.simplevm.parser;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public enum OperatorType 
{
    PLUS("+"),
    MINUS("-"),
    ASSIGNMENT("="),
    ARROW("=>"),
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
    
    public static List<OperatorType> getOperatorTypes(String s) 
    {
        return Arrays.stream( values() ).filter( OperatorType::matchedByLexer ).filter( e -> e.symbol.equals(s ) ).collect(Collectors.toList());
    }    
}
