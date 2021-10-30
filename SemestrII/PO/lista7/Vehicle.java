import java.io.*;

public class Vehicle implements Serializable
{
    protected String manufacturer;
    protected String model;
    protected int seats;

    public Vehicle()
    {
        this.manufacturer = "";
        this.model = "";
        this.seats = 0;
    }

    public String toString()
    {
        return "Manufacturer: " + this.manufacturer + "Model: " + this.model + " Seats: " + Integer.toString(this.seats);
    }

    public String getManufacturer() 
    {
        return this.manufacturer;
    }
    public void setManufacturer(String manufacturer)
    {
        this.manufacturer=manufacturer;
    }
    public String getModel() 
    {
        return this.model;
    }
    public void setModel(String model)
    {
        this.model=model;
    }
    public int getSeats() 
    {
        return this.seats;
    }
    public void setSeats(int seats)
    {
        this.seats=seats;
    }

    public static Vehicle loadFromFile(String fileName)
    {
        Vehicle returnValue;
        File newFile = new File(fileName);
        if(newFile.exists())
        {
            try
            {
                FileInputStream filein = new FileInputStream(newFile);
                ObjectInputStream in = new ObjectInputStream(filein);

                returnValue = (Vehicle)in.readObject();

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
            returnValue = new Vehicle();
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