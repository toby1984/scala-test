package de.codesourcery.simplevm.parser;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.BiFunction;

import de.codesourcery.simplevm.parser.ast.IASTNode;
import de.codesourcery.simplevm.parser.ast.TypedValue;
import scala.Option;

public enum OperatorType
{
    PLUS("+")
    {
        @Override
        public TypeName getType(IASTNode... args)
        {
            TypeName r1 = type( args[0] );
            TypeName r2 = type( args[1] );
            if ( r1 == KnownTypes.INTEGRAL() && r2 == KnownTypes.INTEGRAL() ) {
                return KnownTypes.INTEGRAL();
            }
            return KnownTypes.UNKNOWN();
        }

        @Override
        public TypedValue evaluate(IASTNode... args)
        {
            final BiFunction<Optional<Object>, Optional<Object>, TypedValue> func = (opt1, opt2) ->
            {
                if ( allPresent(opt1,opt2) )
                {
                    final long result = longValue(opt1) + longValue(opt2);
                    return new TypedValue( some(result) ,  KnownTypes.INTEGRAL() );
                }
                return new TypedValue( none() ,  KnownTypes.INTEGRAL() );
            };
            return binaryOp(args[0],args[1] , func );
        }
    },
    MINUS("-")
    {
        @Override
        public TypeName getType(IASTNode... args)
        {
            final TypeName r1 = type( args[0] );
            final TypeName r2 = type( args[1] );
            if ( r1 == KnownTypes.INTEGRAL() && r2 == KnownTypes.INTEGRAL() ) {
                return KnownTypes.INTEGRAL();
            }
            return KnownTypes.UNKNOWN();
        }

        @Override
        public TypedValue evaluate(IASTNode... args)
        {
            final BiFunction<Optional<Object>, Optional<Object>, TypedValue> func = (opt1, opt2) ->
            {
                if ( allPresent(opt1,opt2) )
                {
                    final long result = longValue( opt1 ) - longValue( opt2 );
                    return new TypedValue( some(result) ,  KnownTypes.INTEGRAL() );
                }
                return new TypedValue( none() ,  KnownTypes.INTEGRAL() );
            };
            return binaryOp(args[0],args[1] , func );
        }
    },
    ASSIGNMENT("=") {
        @Override
        public TypeName getType(IASTNode... args)
        {
            return args[0].evaluate().kind();
        }

        @Override
        public TypedValue evaluate(IASTNode... args)
        {
            return args[0].evaluate();
        }
    },
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
    	for ( int i = 0 , len = values().length ; i < len ; i++)
    	{
    		final OperatorType tt = values()[i];
    		if ( tt.matchedByLexer() && tt.symbol.equals( s ) ) {
    			return true;
    		}
    	}
        return false;
    }
    
    public static boolean isValidOperator(char c)
    {
    	for ( int i = 0 , len = values().length ; i < len ; i++)
    	{
    		final OperatorType tt = values()[i];
    		if ( tt.matchedByLexer() && tt.symbol.charAt(0) == c ) {
    			return true;
    		}
    	}
        return false;
    }    

    public static List<OperatorType> getOperatorTypes(String s)
    {
    	final List<OperatorType> result = new ArrayList<>();
    	for ( int i = 0 , len = values().length ; i < len ; i++)
    	{
    		final OperatorType tt = values()[i];
    		if ( tt.matchedByLexer() && tt.symbol.equals( s ) ) {
    			result.add( tt );
    		}
    	}
        return result;
    }

    public TypeName getType(IASTNode... arguments)
    {
        throw new RuntimeException("getType() not implemented for "+this);
    }

    public TypedValue evaluate(IASTNode... arguments)
    {
        throw new RuntimeException("evaluate() not implemented for "+this);
    }

    protected final TypeName type(IASTNode node) {
        return node.evaluate().kind();
    }

    protected final boolean allPresent(Optional<Object> o1,Optional<Object> other) {
        return o1.isPresent() && other.isPresent();
    }

    protected final Optional<Object> value(IASTNode node)
    {
        final TypedValue result = node.evaluate();
        final Option<Object> value = result.value();
        return value.isEmpty() ? Optional.empty() : Optional.of( value.get() );
    }

    protected final long longValue(Optional<Object> obj)
    {
        return ((Number) obj.get() ).longValue();
    }

    protected final TypedValue binaryOp(IASTNode n1 , IASTNode n2 , BiFunction<Optional<Object>,Optional<Object>,TypedValue> func)
    {
        final Optional<Object> opt1 = value( n1 );
        final Optional<Object> opt2 = value( n2 );
        return func.apply(opt1,opt2);
    }

    protected final Option<Object> none() {
        return Option.empty();
    }

    protected final Option<Object> some(Object x) {
        return Option.apply(x);
    }
}