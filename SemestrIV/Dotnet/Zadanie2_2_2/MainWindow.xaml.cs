using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace Zadanie2_2_2 {
    /// <summary>
    /// Logika interakcji dla klasy MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window {

        private MainWindowModel model;

        public MainWindow() {
            this.model = new MainWindowModel();
            this.DataContext = model;
            InitializeComponent();
        }
    }

    public class MainWindowModel : INotifyPropertyChanged {

        public int ProgressBarValue { get; set; }
        public DateTime SelectedDate { get; set; }

        public event PropertyChangedEventHandler PropertyChanged;

        private int _sliderValue;
        public int SliderValue { 
            get {
                return this._sliderValue;
            } 
            set {
                this._sliderValue = value;
                this.SliderValueStr = _sliderValue.ToString();
                PropertyChanged(this, new PropertyChangedEventArgs("SliderValueStr"));
            } 
        }
        public string SliderValueStr { get; set; }

        public ICommand ProgressIncr {
            get {
                return new CustomCommand(
                    x => {
                        this.ProgressBarValue += 10;
                        this.PropertyChanged(this, new PropertyChangedEventArgs("ProgressBarValue"));
                    }
                );
            }
        }
        public ICommand ProgressDecr {
            get {
                return new CustomCommand(
                    x => {
                        this.ProgressBarValue -= 10;
                        this.PropertyChanged(this, new PropertyChangedEventArgs("ProgressBarValue"));
                    }
                );
            }
        }
        public ICommand NextDay {
            get {
                return new CustomCommand(
                    x => {
                        this.SelectedDate = this.SelectedDate.AddDays(1);
                        this.PropertyChanged(this, new PropertyChangedEventArgs("SelectedDate"));
                    }
                );
            }
        }
        public MainWindowModel() {
            this.ProgressBarValue = 50;
            this._sliderValue = 50;
            this.SliderValueStr = this.SliderValue.ToString();
            this.SelectedDate = DateTime.Now;
        }
    }
    public class CustomCommand : ICommand {
        Action<object> _action;

        public CustomCommand(Action<object> action) {
            this._action = action;
        }

        public event EventHandler CanExecuteChanged;

        public bool CanExecute(object parameter) {
            return true;
        }

        public void Execute(object parameter) {
            this._action(parameter);
        }
    }
}
