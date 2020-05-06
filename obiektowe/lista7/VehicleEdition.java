import javax.swing.*;
import java.awt.*;

public class VehicleEdition extends JPanel
{
    private Vehicle vehicle;
    protected GridBagConstraints constraints;
    protected JButton saveButton;
    protected JTextField manField;
    protected JTextField modField;
    protected JTextField seatField;
    protected String filename;

    VehicleEdition(Vehicle vehicle, String filename)
    {
        super(new GridBagLayout());
        this.vehicle=vehicle;
        this.filename=filename;

        manField = new JTextField(15);
        manField.setText(vehicle.getManufacturer());
        modField = new JTextField(15);
        modField.setText(vehicle.getModel());
        seatField = new JTextField(15);
        seatField.setText(Integer.toString(vehicle.getSeats()));

        saveButton = new JButton("Save");

        constraints = new GridBagConstraints();
        constraints.anchor = GridBagConstraints.WEST;
        constraints.insets = new Insets(10,10,10,10);

        constraints.gridx = 0;
        constraints.gridy = 0;
        this.add(new JLabel("Manufacturer: "),constraints);
        constraints.gridx = 1;
        this.add(manField,constraints);

        constraints.gridx = 0;
        constraints.gridy = 1;
        this.add(new JLabel("Model: "),constraints);
        constraints.gridx = 1;
        this.add(modField,constraints);

        constraints.gridx = 0;
        constraints.gridy = 2;
        this.add(new JLabel("Seats: "),constraints);
        constraints.gridx = 1;
        this.add(seatField,constraints);
        constraints.gridx = 0;
        constraints.gridy = 3;

        constraints.gridwidth = 2;
        constraints.anchor = GridBagConstraints.CENTER;
        this.add(saveButton,constraints);

        saveButton.addActionListener(event -> 
        {
            this.vehicle.setManufacturer(manField.getText());
            this.vehicle.setModel(modField.getText());
            this.vehicle.setSeats(Integer.parseInt(seatField.getText()));
            this.vehicle.saveToFile(this.filename);
            System.exit(0);
        });

        this.setBorder(BorderFactory.createTitledBorder(BorderFactory.createEtchedBorder(), "Editing Vehicle"));
    }
}