import javax.swing.*;
import java.awt.*;


public class TramEdition extends VehicleEdition
{
    private Tram tram;
    JTextField carField;
    JTextField lineField;
    JTextField maxCapField;

    TramEdition(Tram tram,String filename)
    {
        super(tram,filename);
        this.tram = tram;
        carField  = new JTextField(15);
        carField.setText(tram.getCarrier());
        lineField = new JTextField(15);
        lineField.setText(Integer.toString(tram.getLineNumber()));
        maxCapField = new JTextField(15);
        maxCapField.setText(Integer.toString(tram.getMaxCapacity()));
        super.remove(this.saveButton);

        this.saveButton = new JButton("Save");

        super.constraints.anchor = GridBagConstraints.WEST;
        super.constraints.gridx = 0;
        super.constraints.gridy = 3;
        super.add(new JLabel("Carrier: "),constraints);
        super.constraints.gridx = 1;
        super.add(carField,constraints);

        super.constraints.gridx = 0;
        super.constraints.gridy = 4;
        super.add(new JLabel("Line Number: "),constraints);
        super.constraints.gridx = 1;
        super.add(lineField,constraints);

        super.constraints.gridx = 0;
        super.constraints.gridy = 5;
        super.add(new JLabel("Max Capacity: "),constraints);
        super.constraints.gridx = 1;
        super.add(maxCapField,constraints);

        constraints.gridx = 0;
        constraints.gridy = 6;

        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.CENTER;
        super.add(saveButton,constraints);

        saveButton.addActionListener(event -> 
        {
            try
            {
                this.tram.setManufacturer(manField.getText());
                this.tram.setModel(modField.getText());
                this.tram.setSeats(Integer.parseInt(seatField.getText()));
                this.tram.setCarrier(carField.getText());
                this.tram.setLineNumber(Integer.parseInt(lineField.getText()));
                this.tram.setMaxCapacity(Integer.parseInt(maxCapField.getText()));
                this.tram.saveToFile(this.filename);
                System.exit(0);
            }
            catch(NumberFormatException e)
            {
                System.out.println("Niepoprawny zapis liczb");
            }
        });

        this.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "Editing Tram"));


    }
}