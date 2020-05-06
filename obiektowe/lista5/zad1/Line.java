
public class Line extends Figure implements Comparable<Figure>
{
    public Point[] coords;
    
    public Line(Point a, Point b)
    {
        this.coords = new Point[2];
        this.coords[0] = a;
        this.coords[1] = b;
    }


    public int compareTo(Figure f)
    {
        if(f instanceof Line) return 0;
        else
        {
            if(coords[0].compareTo(f) >= 0) return 1;
            else return -1;
        }
    }


}