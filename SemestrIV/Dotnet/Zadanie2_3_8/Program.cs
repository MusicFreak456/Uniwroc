using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace Zadanie2_3_8 {
    class Program {
        static void Main(string[] args) {
            Console.OutputEncoding = Encoding.UTF8;
            List<string> cultures = new List<string> { "en", "de", "fr", "ru", "ar", "cs", "pl" };
            List<DayOfWeek> days = new List<DayOfWeek> {
                DayOfWeek.Monday,
                DayOfWeek.Tuesday,
                DayOfWeek.Wednesday,
                DayOfWeek.Thursday,
                DayOfWeek.Friday,
                DayOfWeek.Saturday,
                DayOfWeek.Sunday};
            List<int> months = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };
            DateTime currentDate = DateTime.Now;


            foreach(string culture in cultures) {
                CultureInfo cultureInfo = CultureInfo.GetCultureInfo(culture);
                Console.WriteLine("\nJęzyk: " + cultureInfo.DisplayName);
                DateTimeFormatInfo dateTimeInfo = cultureInfo.DateTimeFormat;

                Console.WriteLine("\nMiesiące: ");

                foreach (int month in months) {
                    Console.WriteLine(
                        dateTimeInfo.GetAbbreviatedMonthName(month) +
                        " " +
                        dateTimeInfo.GetMonthName(month)
                    );
                }

                Console.WriteLine("\nDni tygodnia: ");
                foreach(DayOfWeek day in days) {
                    Console.WriteLine(
                        dateTimeInfo.GetAbbreviatedDayName(day) +
                        " " +
                        dateTimeInfo.GetDayName(day)
                    );
                }

                Console.WriteLine("\nBieżąca data: " +  currentDate.ToString(cultureInfo));

            }
        }
    }
}
