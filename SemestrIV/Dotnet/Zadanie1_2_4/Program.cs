using System;
using System.Collections.Generic;

namespace Zadanie1_2_4 {
    public class ListHelper {
        // Nie jestem pewien czy zrozumiałem  treść  poleceania
        // i chyba nie widzę sensu posiadania klasy ListHelper,
        // moglibyśmy po prostu wywoływać metody  biblioteczne
        // więc zakładam, że chodzi o to żeby  je  zaimplement-
        // -ować od zera.
        public static List<TOutput> ConvertAll<T, TOutput>(
            List<T> list, 
            Converter<T, TOutput> converter
        ) {
            List<TOutput> result = new List<TOutput>();
            foreach(T x in list) {
                result.Add(converter(x));
            }
            return result;
        }
        public static List<T> FindAll<T>(
            List<T> list,
            Predicate<T> match
        ) {
            List<T> result = new List<T>();
            foreach(T x in list) {
                if (match(x)) {
                    result.Add(x);
                }
            }
            return result;
        }
        public static void ForEach<T>(List<T> list, Action<T> action) {
            foreach(T x in list) {
                action(x);
            }
        }
        public static int RemoveAll<T>(
            List<T> list,
            Predicate<T> match) 
        {
            int count = 0;
            int length = list.Count;
            for (int i = 0; i < length; i++) {
                if (match(list[i - count])) {
                    list.RemoveAt(i - count);
                    count++;
                }
            }
                
            return count;
        }
        public static void Sort<T>(
            List<T> list,
            Comparison<T> comparison
        ) {
            int length = list.Count;
            for(int i = 0; i < length-1; i++) {
                for(int j=0; j < length-1-i; j++) {
                    if(comparison(list[j+1], list[j]) == 1) {
                        T temp = list[j];
                        list[j] = list[j+1];
                        list[j+1] = temp;
                    }
                }
            }
        }
    }
    class Program {
        static void prettyPrint(string text, List<int> list) {
            Console.WriteLine("".PadRight(24, '-'));
            Console.WriteLine(text);
            foreach (int x in list) {
                Console.Write(x + " ");
            }
            Console.Write("\n");
        }
        static void Main(string[] args) {
            List<int> Example = new List<int>() { 4, 2, 2, 1, 3, 7, 1, 4, 1, 0 };


            Console.WriteLine("ForEach:");
            ListHelper.ForEach(Example,i => Console.Write(i + " "));
            Console.Write("\n");
            prettyPrint("ConvertAll: *2 ",ListHelper.ConvertAll(Example, x => x * 2));
            prettyPrint("FindAll: nieparzyste", ListHelper.FindAll(Example, x => Convert.ToBoolean(x % 2)));
            ListHelper.RemoveAll(Example, x => x < 2);
            prettyPrint("RemoveAll: <2", Example);
            ListHelper.Sort(Example, (a, b) => {
                if (a > b) return -1;
                else if (a != b) return 1;
                return 0;
            });
            prettyPrint("Sort Desc", Example);
        }
    }
}
