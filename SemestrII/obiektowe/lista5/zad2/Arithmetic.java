
public abstract class Arithmetic extends Expression
{
    protected Expression leftSubexpr;
    protected Expression rightSubexpr;

    public Arithmetic(Expression leftSubexpr, Expression rigtSubexpr)
    {
        this.leftSubexpr=leftSubexpr;
        this.rightSubexpr=rigtSubexpr;
    }

}