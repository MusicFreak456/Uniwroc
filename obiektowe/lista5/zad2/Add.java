import java.util.HashMap;

public class Add extends Arithmetic
{
    public Add(Expression leftSubexpr, Expression rightSubexp)
    {
        super(leftSubexpr,rightSubexp);
    }

    public int eval()
    {
        int leftvalue = leftSubexpr.eval();
        int rightvalue = rightSubexpr.eval();

        return leftvalue + rightvalue;
    }

    public int eval(HashMap<String,Integer> vals)
    {
        int leftvalue = leftSubexpr.eval(vals);
        int rightvalue = rightSubexpr.eval(vals);

        return leftvalue + rightvalue;
    }

    public String toString()
    {
        return leftSubexpr.toString() + " + " +rightSubexpr.toString();
    }
}