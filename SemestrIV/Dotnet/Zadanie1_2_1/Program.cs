using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;

namespace Zadanie1_2_1 {
    class Program {
        static void startup() {
            int result = 0;
            for(int i = 1; i <= 5; i++) {
                for(int j = 1; j <= 1000000; j++) {
                    Random random = new Random();
                    result += random.Next(42);
                }
                Console.Write(result);
            }
            Console.Write('\n');
        }
        static void listComp() {
            Random random = new Random();

            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("LISTY");
            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Dodawanie 1000000 elementów");

            //////Pusty przebieg//////////////////
            ArrayList testedArrayList = new ArrayList();
            for (int j = 1; j <= 1000000; j++) {
                testedArrayList.Add(random.Next(j));
            }
            //////Koniec pustego przebiegu////////

            List<int> testedList = new List<int>();
            DateTime start = DateTime.Now;
            for (int i = 1; i <= 1000000; i++) {
                testedList.Add(random.Next(i));
            }
            DateTime end = DateTime.Now;
            Console.WriteLine("List<T>: " + (end - start).TotalMilliseconds.ToString());


            testedArrayList.Clear();
            start = DateTime.Now;
            for (int i = 1; i <= 1000000; i++) {
                testedArrayList.Add(random.Next(i));
            }
            end = DateTime.Now;
            Console.WriteLine("ArrayList: " + (end - start).TotalMilliseconds.ToString());

            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Przeglądanie 1000000 elementów");

            start = DateTime.Now;
            for (int i = 0; i <= 999999; i++) {
                if (testedList[i] == -1) {
                    Console.WriteLine("cud"); //cuda się nie zdarzają
                }
            }
            end = DateTime.Now;
            Console.WriteLine("List<T>: " + (end - start).TotalMilliseconds.ToString());


            start = DateTime.Now;
            for (int i = 0; i <= 999999; i++) {
                if((int)testedArrayList[i] == -1) {
                    Console.WriteLine("cud"); //cuda się nie zdarzają
                }
            }
            end = DateTime.Now;
            Console.WriteLine("ArrayList: " + (end - start).TotalMilliseconds.ToString());

            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Usuwanie 1000000 elementów");

            start = DateTime.Now;
            for (int i = 999999; i >= 0; i--) {
                testedList.RemoveAt(i);
            }
            end = DateTime.Now;
            Console.WriteLine("List<T>: " + (end - start).TotalMilliseconds.ToString());

            start = DateTime.Now;
            for (int i = 999999; i >= 0; i--) {
                testedArrayList.RemoveAt(i);
            }
            end = DateTime.Now;
            Console.WriteLine("ArrayList: " + (end - start).TotalMilliseconds.ToString());
            Console.WriteLine("".PadRight(24, '-'));

        }

        static void dictComp() {
            Random random = new Random();

            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("SŁOWNIKI");
            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Dodawanie 1000000 elementów");

            //////Pusty przebieg//////////////////
            Hashtable testedHashtable = new Hashtable();
            for (int j = 1; j <= 1000000; j++) {
                testedHashtable.Add(j, j.ToString());
            }
            //////Koniec pustego przebiegu////////

            Dictionary<int,string> testedDict = new Dictionary<int,string>();
            DateTime start = DateTime.Now;
            for (int i = 1; i <= 1000000; i++) {
                testedDict.Add(i, i.ToString());
            }
            DateTime end = DateTime.Now;
            Console.WriteLine("Dictionary<T,K>: " + (end - start).TotalMilliseconds.ToString());


            testedHashtable.Clear();
            start = DateTime.Now;
            for (int i = 1; i <= 1000000; i++) {
                testedHashtable.Add(i, i.ToString());
            }
            end = DateTime.Now;
            Console.WriteLine("Hashtable: " + (end - start).TotalMilliseconds.ToString());

            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Przeglądanie 1000000 elementów");

            start = DateTime.Now;
            for (int i = 1; i <= 1000000; i++) {
                if (testedDict[i] == "-1") {
                    Console.WriteLine("cud"); //cuda się nie zdarzają
                }
            }
            end = DateTime.Now;
            Console.WriteLine("Dictionary<T,K>: " + (end - start).TotalMilliseconds.ToString());


            start = DateTime.Now;
            for (int i = 1; i <= 1000000; i++) {
                if ((string)testedHashtable[i] == "-1") {
                    Console.WriteLine("cud"); //cuda się nie zdarzają
                }
            }
            end = DateTime.Now;
            Console.WriteLine("Hashtable: " + (end - start).TotalMilliseconds.ToString());

            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Usuwanie 1000000 elementów");

            start = DateTime.Now;
            for (int i = 1000000; i >= 1; i--) {
                testedDict.Remove(i);
            }
            end = DateTime.Now;
            Console.WriteLine("Dictionary<T,K>: " + (end - start).TotalMilliseconds.ToString());

            start = DateTime.Now;
            for (int i = 1000000; i >= 1; i--) {
                testedHashtable.Remove(i);
            }
            end = DateTime.Now;
            Console.WriteLine("Hashtable: " + (end - start).TotalMilliseconds.ToString());
            Console.WriteLine("".PadRight(24, '-'));

        }

        static void Main(string[] args) {
            Console.WriteLine("Coś do roboty na rozruch vm:");
            startup();
            listComp();
            dictComp();
        }
    }
}
