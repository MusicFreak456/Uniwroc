
public class Square extends Figure implements Comparable<Figure>
{
    public Line[] side;

    public Square(Line a, Line b, Line c, Line d)
    {
        this.side = new Line[4];
        side[0] = a;
        side[1] = b;
        side[2] = c;
        side[3] = d;
    }

    public int compareTo(Figure f)
    {
        if(f instanceof Square) return 0;
        else
        {
            if( side[0].compareTo(f) >= 0 )return 1;
            else return -1;

        }
    }
    

}