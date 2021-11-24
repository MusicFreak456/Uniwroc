using System;
using System.Collections;

namespace Zadanie2_3_2 {

    class Set : ArrayList {
        public Set() : base() {}

        public override int Add(object value) {
            if(this.IndexOf(value) == -1) {
                base.Add(value);
                return this.IndexOf(value);
            }
            return this.IndexOf(value);
        }
    }

    class Program {
        static void Main(string[] args) {
            Set testSet = new Set();

            testSet.Add(1);
            testSet.Add(2);
            testSet.Add(3);
            testSet.Add(2);
            testSet.Add(4);
            testSet.Add(3);

            foreach (var x in testSet) {
                Console.WriteLine(x.ToString());
            }
        }
    }
}
