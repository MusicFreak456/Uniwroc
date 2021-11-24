using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;

namespace Zadanie1_1_4 {
    
    public class OznakowanieAttribute : Attribute {}

    public class Foo {
        [Oznakowanie]
        public int Bar() {
            return 1;
        }
        public int Qux() {
            return 2;
        }

        private int test() {
            return 3;
        }

        public int test2(int x) {
            return x;
        }

    }

    class Program {

        static void pub_int_methods(object o) {
            MethodInfo[] methods = o.GetType().GetMethods(BindingFlags.Public | BindingFlags.Instance).Where(x => {
                if (x.GetParameters().Length == 0 && x.ReturnType == typeof(int)) return true;
                return false;
            }).ToArray();

            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Znalezione metody:");
            foreach (MethodInfo method in methods) {
                Console.WriteLine(method);
            }
            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine("Wywołanie oznakowanych metod:");

            foreach (MethodInfo method in methods) {
                if (method.GetCustomAttribute(typeof(OznakowanieAttribute)) != null) {
                    Console.WriteLine(method.Invoke(o,null));
                }
            }
        }

        static void Main(string[] args) {
            Foo bar = new Foo();
            pub_int_methods(bar);
        }
    }
}
