import javax.swing.*;
import java.awt.*;


public class CarEdition extends VehicleEdition
{
    private Car car;
    JTextField milField;
    JTextField engpowField;
    JTextField engcapField;

    CarEdition(Car car,String filename)
    {
        super(car,filename);
        this.car = car;
        milField    = new JTextField(15);
        milField.setText(Integer.toString(car.getMileage()));
        engpowField = new JTextField(15);
        engpowField.setText(Integer.toString(car.getEnginePower()));
        engcapField = new JTextField(15);
        engcapField.setText(Integer.toString(car.getEngineCapacity()));
        super.remove(this.saveButton);

        this.saveButton = new JButton("Save");

        super.constraints.anchor = GridBagConstraints.WEST;
        super.constraints.gridx = 0;
        super.constraints.gridy = 3;
        super.add(new JLabel("Mileage: "),constraints);
        super.constraints.gridx = 1;
        super.add(milField,constraints);

        super.constraints.gridx = 0;
        super.constraints.gridy = 4;
        super.add(new JLabel("Engine Power: "),constraints);
        super.constraints.gridx = 1;
        super.add(engpowField,constraints);

        super.constraints.gridx = 0;
        super.constraints.gridy = 5;
        super.add(new JLabel("Engine Capacity: "),constraints);
        super.constraints.gridx = 1;
        super.add(engcapField,constraints);

        constraints.gridx = 0;
        constraints.gridy = 6;

        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.CENTER;
        super.add(saveButton,constraints);

        saveButton.addActionListener(event -> 
        {
            try
            {
                this.car.setManufacturer(manField.getText());
                this.car.setModel(modField.getText());
                this.car.setSeats(Integer.parseInt(seatField.getText()));
                this.car.setMileage(Integer.parseInt(milField.getText()));
                this.car.setEnginePower(Integer.parseInt(engpowField.getText()));
                this.car.setEngineCapacity(Integer.parseInt(engcapField.getText()));
                this.car.saveToFile(this.filename);
                System.exit(0);
            }
            catch(NumberFormatException e)
            {
                System.out.println("Niepoprawny zapis liczb");
            }
        });

        this.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "Editing Car"));


    }
}