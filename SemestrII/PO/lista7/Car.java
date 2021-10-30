import java.io.*;

public class Car extends Vehicle implements Serializable
{
    protected int mileage;
    protected int enginePower;
    protected int engineCapacity;

    public Car()
    {
        super();
        this.mileage=0;
        this.engineCapacity=0;
        this.enginePower=0;
    }

    public String toString()
    {
        return super.toString() + " Milage: " + Integer.toString(this.mileage) +
         " Engine Capacity: " + Integer.toString(this.engineCapacity) + " Engine Power: " +
        Integer.toString(this.enginePower);
    }

    public int getMileage() 
    {
        return this.mileage;
    }
    public void setMileage(int mileage)
    {
        this.mileage=mileage;
    }
    public int getEngineCapacity() 
    {
        return this.engineCapacity;
    }
    public void setEngineCapacity(int engineCapacity)
    {
        this.engineCapacity=engineCapacity;
    }
    public int getEnginePower() 
    {
        return this.enginePower;
    }
    public void setEnginePower(int enginePower)
    {
        this.enginePower=enginePower;
    }

    public static Car loadFromFile(String fileName)
    {
        Car returnValue;
        File newFile = new File(fileName);
        if(newFile.exists())
        {
            try
            {
                FileInputStream filein = new FileInputStream(newFile);
                ObjectInputStream in = new ObjectInputStream(filein);

                returnValue = (Car)in.readObject();

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
            returnValue = new Car();
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