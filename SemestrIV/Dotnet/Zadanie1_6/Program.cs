using System;

namespace Zadanie1_6 {

    delegate void ExampleDelegate(int x, int y);

    class ExampleClass {
        private string name;
        private int _exampleProperty;
        private int[] exampleTable;
        public int changeCount;
        public event ExampleDelegate ExampleEvent;


        public ExampleClass(string name) {
            this.name = name;
            this.changeCount = 0;
            this.exampleTable = new int[5] { 1, 2, 3, 4, 5 };
        }
        public int ExampleProperty {
            get {
                return this._exampleProperty;
            }
            set {
                this.changeCount++;
                this._exampleProperty = value;
            }
        }

        public int this[int x] {
            get {
                return this.exampleTable[x];
            }
            set {
                this.exampleTable[x] = value;
            }
        }

        public void DisplayName() {
            Console.WriteLine(this.name);
        }

        public void CallEvent(int x, int y) {
            this.ExampleEvent(x, y);
        }

    }

    class Functions {
        public static void AddAndDisplay(int x, int y) {
            Console.WriteLine($"{x} + {y} = {x + y}");
        }

        public static void SubtractAndDisplay(int x, int y) {
            Console.WriteLine($"{x} - {y} = {x - y}");
        }
        public static void CalculateLCMAndDisplay(int x, int y) {
            int result = x;
            int temp = y;

            while(result != temp) {
                if(result < temp) {
                    result += x;
                }
                else {
                    temp += y;
                }
            }

            Console.WriteLine($"LCM({x},{y}) = {result}");
        }
    }

    class Program {
        static void Main(string[] args) {
            Console.WriteLine("\nPola i metody");
            ExampleClass exampleObject = new ExampleClass("PrzykładowyObiekt");
            exampleObject.DisplayName();

            Console.WriteLine("\nPropercje i indeksery");
            exampleObject.ExampleProperty = 1;
            Console.WriteLine(exampleObject.ExampleProperty);
            Console.WriteLine(exampleObject.changeCount);
            Console.WriteLine(exampleObject[4]);

            Console.WriteLine("\nDelegacje i zdarzenia");
            exampleObject.ExampleEvent += Functions.AddAndDisplay;
            exampleObject.ExampleEvent += Functions.SubtractAndDisplay;
            exampleObject.ExampleEvent += Functions.CalculateLCMAndDisplay;
            exampleObject.CallEvent(5, 3);

            Console.WriteLine("\nPodstawowe konstrukcje składniowe");

            int i = 1;
            while(i<=6) {
                Console.Write(i.ToString() + ": ");
                switch (i % 2) {
                    case 0:
                        Console.Write("parzysta, ");
                        break;
                    case 1:
                        Console.Write("nieparzysta, ");
                        break;
                }
                if (i > 5) Console.Write("większa od 5\n");
                else Console.Write("mniejsza od 5\n");
                i++;
            }
        }
    }
}
