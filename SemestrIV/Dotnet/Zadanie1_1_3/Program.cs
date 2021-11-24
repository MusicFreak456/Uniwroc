using System;
using System.Collections.Generic;
using System.Reflection;

namespace Zadanie1_1_3 {
       
    class ExampleClass {
        private string privateField;
        public int PublicField;

        public ExampleClass(string privateString) {
            this.privateField = privateString;
            this.PublicField = 42;
        }

        public void privateMethod() {
            Console.WriteLine("Metoda prywatna");
        }
    }

    class Program {
        static void accessPrivateComponents(object o) {
            Type objectType = o.GetType();
            foreach(FieldInfo x in objectType.GetFields(BindingFlags.Instance | BindingFlags.NonPublic)) {
                Console.WriteLine("Pole: " + x.Name + "\nWartość: " + x.GetValue(o));
            }
        }

        static void compare() {
            Console.WriteLine("Wywołania na rozruch:");
            for(int i=1; i<=5; i++) {
                test("regular");
                test("reflection");
            }
            Console.Write("\n");
            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Początek porównania:");
            List<Tuple<double,double>> results = new List<Tuple<double,double>>();
            for (int i = 1; i <= 5; i++) {
                double result_regular = test("regular").TotalMilliseconds;
                double result_reflection = test("reflection").TotalMilliseconds;
                Tuple<double, double> result = new Tuple<double, double>(result_regular,result_reflection);
                results.Add(result);
            }
            double average = 0;

            foreach(Tuple<double,double> result in results) {
                average += 1 - (result.Item1 / result.Item2);
            }

            Console.Write("\n");
            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Kod z refleksją wolniejszy o średnio: " + ( (int)((average / results.Count) * 100)).ToString() + "%" );
        }

        static TimeSpan test(string mode) {
            ExampleClass exampleObject = new ExampleClass("test");
            FieldInfo field = exampleObject.GetType().GetField("PublicField");
            int test = 0;
            DateTime start = DateTime.Now;

            if(mode == "regular") {
                for (int i = 1; i <= 1000000; i++) {
                    int regularAccess = exampleObject.PublicField;
                    Random random = new Random();
                    test += regularAccess + random.Next(42);
                }
            } else {
                for (int i = 1; i <= 1000000; i++) {
                    int accessByReflection = (int)field.GetValue(exampleObject);
                    Random random = new Random();
                    test += accessByReflection + random.Next(42);
                }
            }

            DateTime end = DateTime.Now;
            Console.Write(test);

            return end - start;
        }

        static void Main(string[] args) {
            ExampleClass exampleObject = new ExampleClass("prywatne pole");
            Console.WriteLine("".PadRight(24, '-'));
            accessPrivateComponents(exampleObject);
            Console.WriteLine("".PadRight(24, '-'));
            FieldInfo field = exampleObject.GetType().GetField("publicField");
            compare();
        }
    }
}
