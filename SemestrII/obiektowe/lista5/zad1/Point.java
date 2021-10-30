
public class Point extends Figure implements Comparable<Figure>
{
    private int[] coords;

    public Point(int... vals)
    {
        int index=0;
        this.coords = new int[vals.length];
        for(int x: vals)
        {
            this.coords[index] = x;
            index++;
        } 
    }

    public void display()
    {
        for(int x: coords)
        {
            System.out.println(x);
        }
    }

    public int compareTo(Figure o)
    {
        if(o instanceof Point) return 0;
        else return -1;
    }

}