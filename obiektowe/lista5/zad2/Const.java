import java.util.HashMap;

public class Const extends Expression
{
    private int value;

    public Const(int value)
    {
        this.value=value;
    }

    public int eval()
    {
        return value;
    }

    public int eval(HashMap<String,Integer> vals)
    {
        return this.eval();
    }

    public String toString()
    {
        return Integer.toString(value);
    }
}