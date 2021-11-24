using System;
using System.Linq;
using System.IO;

namespace Zadanie1_3_2 {
    class Program {

        public static long getTotalFilesLength(string directoryName) {
            string[] files = Directory.GetFiles(directoryName);

            return files
                .Select(x => new FileInfo(x).Length)
                .Aggregate((long)0, (x, y) => x + y);
        }

        static void Main(string[] args) {
            Console.WriteLine(getTotalFilesLength("."));
        }
    }
}
