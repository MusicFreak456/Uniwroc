using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.ServiceProcess;
using System.Text;
using System.Threading.Tasks;

namespace Zadanie2_3_9 {
    public partial class Service1 : ServiceBase {

        System.Timers.Timer timer;

        public Service1() {
            InitializeComponent();
        }

        public void Start() {
            this.OnStart(null);
        }

        protected override void OnStart(string[] args) {
            this.timer = new System.Timers.Timer(1000 * 5);
            timer.Elapsed += (s, e) => {
                try {
                    timer.Stop();
                    Process[] processes = Process.GetProcesses();

                    using (FileStream oFileStream = new FileStream("log.txt", FileMode.Append))
                    using (StreamWriter streamWriter = new StreamWriter(oFileStream)) {
                        streamWriter.WriteLine("\n Dane z: " + DateTime.Now.ToString());
                        foreach(Process process in processes) {
                            streamWriter.WriteLine(process.ProcessName);
                        }
                    }
                }
                catch(Exception ex) {
                    Console.WriteLine(ex.Message);
                }
                finally {
                    timer.Start();
                }
            };
            timer.Start();
        }

        protected override void OnStop() {
            this.timer.Stop();
        }
    }
}
