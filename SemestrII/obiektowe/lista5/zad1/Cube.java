
public class Cube extends Figure implements Comparable<Figure>
{
    public Square[] sides;

    public Cube(Square a,Square b,Square c,Square d,Square e,Square f)
    {
        this.sides = new Square[6];
        this.sides[0] = a;
        this.sides[1] = b;
        this.sides[2] = c;
        this.sides[3] = d;
        this.sides[4] = e;
        this.sides[5] = f;
    }

    public int compareTo(Figure f)
    {
        if(f instanceof Cube) return 0;
        else
        {
            if(sides[0].compareTo(f) >= 0) return 1;
            else return -1;
        }
        
    }
}