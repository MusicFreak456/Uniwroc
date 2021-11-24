using System;
using System.Collections.Generic;

namespace Zadanie1_2 {
    /// <summary>
    /// Implementuje siatkę
    /// </summary>
    public class Grid {
        private List<List<int>> grid;
        /// <summary>
        /// Konstruktor klasy Grid
        /// </summary>
        /// <param name="rows">wiersze</param>
        /// <param name="colums">kolumny</param>
        public Grid(int rows, int colums) {
            grid = new List<List<int>>(rows);

            for (int i = 0; i < rows; i++) {
                grid.Add(new List<int>(colums));
                for (int j = 0; j < colums; j++) {
                    grid[i].Add(0);
                }
            }
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="row">indeks wiersza</param>
        /// <returns>wiersz o zadanym indeksie</returns>
        public int[] this[int row] {
            get {
                return this.grid[row].ToArray();
            }
        }
        /// <summary>
        /// 
        /// </summary>
        /// <param name="row">indeks wiersza</param>
        /// <param name="column">indeks kolumny</param>
        /// <returns>zwraca element spod zadanego wiersza i kolumny</returns>
        public int this[int row, int column] {
            get {
                return this.grid[row][column];
            }
            set {
                this.grid[row][column] = value;
            }
        }

    }
    /// <summary>
    /// 
    /// </summary>
    class Program {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="args"></param>
        static void Main(string[] args) {
            Grid testGrid = new Grid(4,4);

            Console.WriteLine(testGrid[0, 0]);
            testGrid[0, 0] = 1;
            testGrid[0, 1] = 2;
            testGrid[1, 0] = 1;
            Console.WriteLine(testGrid[0, 0]);
            Console.WriteLine(string.Join(", ", testGrid[0]));
            Console.WriteLine(string.Join(", ", testGrid[1]));
        }
    }
}
