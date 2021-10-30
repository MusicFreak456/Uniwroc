import java.util.HashMap;

public class Program
{
    public static void main(String[] args) 
    {
        Expression newAddition = new Add(new Const(12), new Const(30));
        System.out.println(newAddition);
        System.out.println(newAddition.eval());

        Expression newSubtraction = new Sub(new Const(50), new Const(8));
        System.out.println(newSubtraction);
        System.out.println(newSubtraction.eval());

        Expression newMultiplication = new Mult(new Const(6), new Const(7));
        System.out.println(newMultiplication);
        System.out.println(newMultiplication.eval());

        Expression newDivision = new Div(new Const(84), new Const(2));
        System.out.println(newDivision);
        System.out.println(newDivision.eval());

        Expression newWithVar = new Add(new Const(2), new Mult(new Var("x"), new Var("y")));
        System.out.println(newWithVar);
        HashMap<String,Integer> vals = new HashMap<String,Integer>();
        vals.put("x",8);
        vals.put("y",5);
        System.out.println(newWithVar.eval(vals));

        try
        {
            Expression notOkDiv = new Div(new Const(42),new Const(0));
            notOkDiv.eval();
        }
        catch(ArithmeticException e)
        {
            System.out.println(e.getMessage());
        }
        try
        {
            newWithVar.eval();
        }
        catch(ArithmeticException e)
        {
            System.out.println(e.getMessage());
        }
        try
        {
            Expression newWithVarTest = new Add(new Const(2), new Mult(new Var("x"), new Var("y")));
            HashMap<String,Integer> valsTest = new HashMap<String,Integer>();
            valsTest.put("x",8);
            System.out.println(newWithVarTest.eval(valsTest));
        }
        catch(ArithmeticException e)
        {
            System.out.println(e.getMessage());
        }

    }
}