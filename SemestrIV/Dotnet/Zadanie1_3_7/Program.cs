using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Zadanie1_3_7 {
    class Program {
        static void Main(string[] args) {
            var item = new { Field1 = "The value", Field2 = 5 };
            var theList = new[] { item }.ToList(); // kompilator sam wydedukuje typ i go nazwie (widać to w podpowiedzi)

            var itemOfTheSameType = new { Field1 = "The value2", Field2 = 6 };
            theList.Add(itemOfTheSameType); // zadziała

            var itemOfADifferentType = new { Field1 = "The value2", Field2 = "Test" };
            //theList.Add(itemOfADifferentType);   // nie zadziała

            var test  = theList[0].Field1; // podpowiedzi tutaj działają
        }
    }
}
