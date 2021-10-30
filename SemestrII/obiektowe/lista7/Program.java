import javax.sql.rowset.spi.SyncFactoryException;
import javax.swing.*;
import java.awt.*;
 
public class Program {
    public static void main(String[] args) 
    {
        if(args.length != 2)
        {
            System.out.println("Usage: Program [Path to file] [Class]");
        }
        JFrame frame = new JFrame("Edycja obiektów");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setLocationRelativeTo(null);
        Container newContainer = frame.getContentPane();
        newContainer.setLayout(new FlowLayout());

        String filename = args[0];
        String className = args[1];

      

        if(className.compareTo("Vehicle") == 0)
        {
            Vehicle vehicle = Vehicle.loadFromFile(filename);
            VehicleEdition edit = new VehicleEdition(vehicle,filename);
            newContainer.add(edit);
        }
        else if(className.compareTo("Car") == 0)
        {
            Car vehicle = Car.loadFromFile(filename);
            CarEdition edit = new CarEdition(vehicle,filename);
            newContainer.add(edit);
        }
        else if(className.compareTo("Tram") == 0)
        {
            Tram vehicle = Tram.loadFromFile(filename);
            TramEdition edit = new TramEdition(vehicle,filename);
            newContainer.add(edit);
        }
        frame.pack();
        frame.setVisible(true);

    }
}