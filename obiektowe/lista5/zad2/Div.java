import java.util.HashMap;

public class Div extends Arithmetic
{
    public Div(Expression leftSubexpr, Expression rightSubexp)
    {
        super(leftSubexpr,rightSubexp);
    }

    public int eval() throws ArithmeticException
    {
        int leftvalue = leftSubexpr.eval();
        int rightvalue = rightSubexpr.eval();

        if(rightvalue==0) throw new ArithmeticException("Division by zero");

        return leftvalue / rightvalue;
    }

    public int eval(HashMap<String,Integer> vals)
    {
        int leftvalue = leftSubexpr.eval(vals);
        int rightvalue = rightSubexpr.eval(vals);

        if(rightvalue==0) throw new ArithmeticException("Division by zero");

        return leftvalue / rightvalue;
    }

    public String toString()
    {
        return leftSubexpr.toString() + " / " +rightSubexpr.toString();
    }
}