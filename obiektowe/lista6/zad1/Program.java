import java.io.*;

public class Program
{
    public static void main(String[] args) 
    {
        Lista<String> nowa = new Lista<String>();

        nowa.addFront("First");
        nowa.addFront("Second");
        nowa.addBack("Third");
        nowa.printList();

        try
        {
            FileOutputStream file = new FileOutputStream("zapis.ser");
            ObjectOutputStream out = new ObjectOutputStream(file);

            out.writeObject(nowa);

            out.close();
            file.close();
            System.out.println("Zapisano");            
        }
        catch (IOException i)
        {
            i.printStackTrace();
        }
        Lista<Integer> wczytana = null;
        try
        {
            FileInputStream ifile = new FileInputStream("zapis.ser");
            ObjectInputStream in = new ObjectInputStream(ifile);

            wczytana = (Lista<Integer>) in.readObject();
            in.close();
            ifile.close();
            System.out.println("Wczytano:");
        }
        catch (IOException i)
        {
            i.printStackTrace();
            return;
        }
        catch (ClassNotFoundException c)
        {
            c.printStackTrace();
            return;
        }

        wczytana.printList();


    }
}