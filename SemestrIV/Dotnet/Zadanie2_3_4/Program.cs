using System;
using System.Collections.Generic;
using System.Threading;

namespace Zadanie2_3_4 {
 
    class Program {

        class BarberShop {
            private const int maxSeets = 5;

            private Semaphore customers = new Semaphore(0, maxSeets);
            private Semaphore barberReady = new Semaphore(0, 1);

            private Mutex accessTakenSeets = new Mutex();
            private int takenSeets = 0;

            private const int totalClients = 10;

            private Random random = new Random();

            public void Barber() {
                while(true) {
                    Console.WriteLine("Barber czeka na klientów");
                    this.customers.WaitOne();

                    this.accessTakenSeets.WaitOne();
                    Console.WriteLine("Zwolnione miejsce w kolejce");
                    this.takenSeets--;
                    this.accessTakenSeets.ReleaseMutex();
                    Thread.Sleep(random.Next(1));
                    this.barberReady.Release();
                    Console.WriteLine("Klient ostrzyżony");
                }
            }

            public void Customer(object id) {
                Thread.Sleep(random.Next(20));
                this.accessTakenSeets.WaitOne();
                Console.WriteLine("Klient {0} sprawdza stan kolejki: {1}", id.ToString(), this.takenSeets);
                if ( takenSeets < maxSeets) {
                    takenSeets++;
                    this.accessTakenSeets.ReleaseMutex();
                    this.customers.Release();
                    Console.WriteLine("Klient {0} dołączył do kolejki", id.ToString());
                    this.barberReady.WaitOne();
                }
                else {
                    this.accessTakenSeets.ReleaseMutex();
                    Console.WriteLine("Kolejka pełna klient {0} zrezygnował", id.ToString());
                }
            }

            public void Run() {
                Thread barber = new Thread(new ThreadStart(this.Barber));
                List<Thread> customers = new List<Thread>();
                for(int i = 1; i <= totalClients; i++) {
                    customers.Add(new Thread(new ParameterizedThreadStart(this.Customer)));
                }

                barber.IsBackground = true;
                barber.Start();

                for (int i = 0; i < totalClients; i++) {
                    customers[i].Start(i);
                }
            }
        }
        

        static void Main(string[] args) {
            BarberShop barberShop = new BarberShop();
            barberShop.Run();
        }
    }
}
