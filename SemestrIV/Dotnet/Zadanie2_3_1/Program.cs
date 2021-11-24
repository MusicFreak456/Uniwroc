using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Zadanie2_3_1 {

    class Complex : IFormattable {
        private double realPart;
        private double imaginaryPart;

        public Complex(double r, double i) {
            this.realPart = r;
            this.imaginaryPart = i;
        }

        public override string ToString() {
            return this.ToString(null);
        }

        public string ToString(string format) {
            return this.ToString(format, null);
        }

        public string ToString(string format, IFormatProvider formatProvider) {
            if (format == null) format = "";
            if (formatProvider == null) formatProvider = CultureInfo.CurrentCulture;

            string realPart = this.realPart.ToString(formatProvider);
            string imaginaryPart = this.imaginaryPart.ToString(formatProvider);

            switch (format.ToUpperInvariant()) {
                case "":
                case "D":
                default:
                    return realPart + " + " + imaginaryPart + "i";
                case "W":
                    return "[" + realPart + "," + imaginaryPart + "]";

            }
        }
    }
    class Program {

        static void Main(string[] args) {
            Complex z = new Complex(4, 3);
            Console.WriteLine(String.Format("{0}", z));
            Console.WriteLine(String.Format("{0:d}", z));
            Console.WriteLine(String.Format("{0:w}", z));
        }
    }
}
