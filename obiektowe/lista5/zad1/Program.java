
public class Program 
{
    public static void main(String[] args) 
    {
        AscList<Figure> newList = new AscList<Figure>();

        
        Point a = new Point(1,1);
        Point b = new Point(2,2);
        Point c = new Point(1,0);
        Point d = new Point(2,0);

        Line l = new Line(a,b);
        Line k = new Line(b,c);
        Line m = new Line(c,d);
        Line n = new Line(d,a);

        Square sq  = new Square(l, k, m, n);
        Square sq1 = new Square(l, k, m, n);
        Square sq2 = new Square(l, k, m, n);
        Square sq3 = new Square(l, k, m, n);
        Square sq4 = new Square(l, k, m, n);
        Square sq5 = new Square(l, k, m, n);
        Cube cb = new Cube(sq, sq1, sq2, sq3, sq4, sq5);

        newList.add(l);
        newList.add(cb);
        newList.add(a);
        newList.add(sq);

        System.out.println(newList.getElem());
        System.out.println(newList.getElem());
        System.out.println(newList.getElem());
        System.out.println(newList.getElem());
        
    }
}