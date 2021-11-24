using System;
using System.Collections.Generic;

namespace Zadanie1_2 {
    class Grid {
        private List<List<int>> grid;

        public Grid(int rows, int colums) {
            grid = new List<List<int>>(rows);

            for(int i=0; i<rows; i++) {
                grid.Add(new List<int>(colums));
                for(int j=0; j<colums; j++) {
                    grid[i].Add(0);
                }
            }
        }

        public int[] this[int row] {
            get {
                return this.grid[row].ToArray();
            }
        }

        public int this[int row, int column] {
            get {
                return this.grid[row][column];
            }
            set {
                this.grid[row][column] = value;
            }
        }

    }

    class Program {
        static void Main(string[] args) {
            Grid testGrid = new Grid(4, 4);

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
