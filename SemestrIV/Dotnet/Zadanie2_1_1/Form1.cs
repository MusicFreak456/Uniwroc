using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Zadanie2_1_1 {

    public partial class Form1 : Form {

        public Result Result;
        public Form1() {
            Result = new Result();
            InitializeComponent();
        }

        private void btCancel_Click(object sender, EventArgs e) {
            this.DialogResult = DialogResult.Cancel;
            this.Close();
        }

        private void btAccept_Click(object sender, EventArgs e) {
            this.DialogResult = DialogResult.OK;
            this.Close();
        }

        private void cmbCycle_SelectedIndexChanged(object sender, EventArgs e) {
            ComboBox comboBox = sender as ComboBox;
            this.Result.Cycle = comboBox.Text;
        }

        private void chbFull_CheckedChanged(object sender, EventArgs e) {
            CheckBox checkBox = sender as CheckBox;

            if (checkBox.Checked) {
                this.chbCompl.Checked = false;
                this.Result.Full = true;
            }
        }

        private void chbCompl_CheckedChanged(object sender, EventArgs e) {
            CheckBox checkBox = sender as CheckBox;

            if (checkBox.Checked) {
                this.chbFull.Checked = false;
                this.Result.Full = true;
            }
        }

        private void txtName_TextChanged(object sender, EventArgs e) {
            TextBox textBox = sender as TextBox;
            this.Result.Name = textBox.Text;
        }

        private void txtAddress_TextChanged(object sender, EventArgs e) {
            TextBox textBox = sender as TextBox;
            this.Result.Address = textBox.Text;
        }
    }
    public struct Result {
        public string Name { get; set; }
        public string Address { get; set; }

        public string Cycle { get; set; }

        public bool Full { get; set; }
    }
}
