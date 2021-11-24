using System;
using System.Windows.Forms;

namespace Zadanie2_1_1 {
    static class Program {
        [STAThread]
        static void Main() {
            Application.SetHighDpiMode(HighDpiMode.SystemAware);
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            Form1 form1 = new Form1();
            var result = form1.ShowDialog();

            if (result == DialogResult.OK) {
                Result formResult = form1.Result;
                string isFull = formResult.Full ? "dzienne" : "uzupe³niaj¹ce";
                string[] resultStrings = new string[4] { formResult.Name, formResult.Address, formResult.Cycle, isFull };
                string message = string.Join('\n', resultStrings);
                MessageBox.Show(message);
            }
        }
    }
}
