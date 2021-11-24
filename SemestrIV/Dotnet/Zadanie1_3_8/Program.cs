using System;
using System.Linq;
using System.Collections.Generic;

namespace Zadanie1_3_8 {
    class Program {
        
        public static Func<T, TResult> Y<T, TResult>(Func<Func<T, TResult>, Func<T, TResult>> f) {
            Func<T, TResult> g = null;
            g = f(x => g(x));
            return g;
        }

        static void Main(string[] args) {
            List<int> list = new List<int>() { 1, 2, 3, 4, 5 };
            foreach (var item in
            list.Select(i => Y<int,int>( e => x => (x <= 2)? 1 : e(x-1) + e(x-2))(i))) {
                Console.WriteLine(item);
            }
        }
    }
}
