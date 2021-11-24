using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Net.Http;
using System.Net;

namespace Zadanie2_1_7 {
    public partial class Form1 : Form {
        private string url;
        private HttpClient httpClient;
        private WebClient webClient;
        private Random random;

        public Form1() {
            InitializeComponent();
            this.url = "https://skos.ii.uni.wroc.pl/";
            this.httpClient = new HttpClient();
            this.webClient = new WebClient();
            this.random = new Random();
        }

        private void timer1_Tick(object sender, EventArgs e) {
            if (this.progressBar1.Value >= 100) this.progressBar1.Value = 0;
            this.progressBar1.Value++;
        }

        private void button1_Click(object sender, EventArgs e) {
            this.httpClient.GetStringAsync(this.url);
        }

        private void button2_Click(object sender, EventArgs e) {
            this.webClient.DownloadString(this.url);
        }
    }
}
