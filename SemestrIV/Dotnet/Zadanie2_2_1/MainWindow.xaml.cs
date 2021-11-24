using System.Collections.Generic;
using System.ComponentModel;
using System.Windows;

namespace Zadanie2_2_1 {
    /// <summary>
    /// Logika interakcji dla klasy MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window {

        private readonly MainWindowModel model;

        public MainWindow() {
            this.model = new MainWindowModel();
            this.DataContext = this.model;
            this.InitializeComponent();
        }

        private void AcceptClicked(object sender, RoutedEventArgs e) {
            this.Close();
            string[] resultStrings = new string[] { model.Name, model.Address, model.SelectedCycle, model.Type };
            string message = string.Join("\n", resultStrings);
            MessageBox.Show(message);
        }

        private void CancelClicked(object sender, RoutedEventArgs e) {
            this.Close();
        }
    }

    public class MainWindowModel : INotifyPropertyChanged {

        public string Name { get; set; }
        public string Address { get; set; }

        public List<string> CycleCollection {
            get {
                return new List<string> { "3 letnie", "3,5 letnie", "5 letnie" };
            } 
        }

        public event PropertyChangedEventHandler PropertyChanged;

        public string SelectedCycle { get; set; }

        public string Type = "";
        private bool _full;
        public bool Full {
            get { return this._full; }
            set {
                this._full = value;
                if(value == true) {
                    this.Type = "dzienne";
                    this._part = false;
                    PropertyChanged(this,new PropertyChangedEventArgs("Part"));
                }
            }
        }

        private bool _part;
        public bool Part {
            get { return this._part; }
            set {
                this._part = value;
                if(value == true) {
                    this.Type = "uzupełniające";
                    this._full = false;
                    PropertyChanged(this, new PropertyChangedEventArgs("Full"));
                }
            }
        }


        public MainWindowModel() {
        }
    }
}
