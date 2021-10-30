import java.util.HashMap;

public abstract class Expression
{
    abstract public int eval();
    abstract public int eval(HashMap<String,Integer> vals);
    abstract public String toString();

}