import java.util.HashMap;

public class Var extends Expression
{
    private String name;

    public Var(String name)
    {
        this.name = name;
    }

    public int eval() throws ArithmeticException
    {
        throw new ArithmeticException("Variable \"" + name + "\" present, but no value given");
    }

    public int eval(HashMap<String,Integer> vals)
    {
        if(!vals.containsKey(name)) throw new ArithmeticException("Variable \"" + name + "\" present, but no value given");
        int value = vals.get(name);

        return value;
    }

    public String toString()
    {
        return name;
    }

}