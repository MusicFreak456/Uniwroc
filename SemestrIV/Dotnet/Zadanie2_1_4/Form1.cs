using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace Zadanie2_1_4 {
    public partial class Form1 : Form {
        public Form1() {
            InitializeComponent();
        }

        private void timer1_Tick(object sender, EventArgs e) {
            this.smoothProgressBar1.Value += 1;
        }
    }
}
