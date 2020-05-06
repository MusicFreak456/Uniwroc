import java.io.*;

public class Tram extends Vehicle implements Serializable
{
    private String carrier;
    private int lineNumber;
    private int maxCapacity;

    public Tram()
    {
        this.carrier = "";
        this.lineNumber = 0;
        this.maxCapacity = 0;
    }

    public String toString()
    {
        return super.toString() + " Carrier: " + this.carrier +
         " Line Number: " + Integer.toString(this.lineNumber) + " Max Capacity: " +
        Integer.toString(this.maxCapacity);
    }

    public String getCarrier() 
    {
        return this.carrier;
    }
    public void setCarrier(String carrier)
    {
        this.carrier=carrier;
    }
    public int getLineNumber() 
    {
        return this.lineNumber;
    }
    public void setLineNumber(int lineNumber)
    {
        this.lineNumber=lineNumber;
    }
    public int getMaxCapacity() 
    {
        return this.maxCapacity;
    }
    public void setMaxCapacity(int maxCapacity)
    {
        this.maxCapacity=maxCapacity;
    }

    public static Tram loadFromFile(String fileName)
    {
        Tram returnValue;
        File newFile = new File(fileName);
        if(newFile.exists())
        {
            try
            {
                FileInputStream filein = new FileInputStream(newFile);
                ObjectInputStream in = new ObjectInputStream(filein);

                returnValue = (Tram)in.readObject();

                in.close();
                filein.close();
                return returnValue;
            }
            catch(IOException ex) 
            { 
                System.out.println("IOException is caught"); 
            }
            catch(ClassNotFoundException ex) 
            { 
                System.out.println("ClassNotFoundException is caught"); 
            } 
        }
        else
        {
            returnValue = new Tram();
            return returnValue;
        }

        return null;
    }

    public void saveToFile(String fileName)
    {
        try
        {
            FileOutputStream fileout = new FileOutputStream(fileName);
            ObjectOutputStream out = new ObjectOutputStream(fileout);
            
            out.writeObject(this);

            out.close();
            fileout.close();

        }
        catch(IOException ex) 
        { 
            System.out.println("IOException is caught"); 
        }
    }
}