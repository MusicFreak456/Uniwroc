using System;
using System.IO;

namespace Zadanie1_4_2 {
    class Program {
        static void Main(string[] args) {
            string current_path = Directory.GetCurrentDirectory();
            string save_path = Path.Combine(current_path, "document.doc");

            Type typeOfWordApplication = Type.GetTypeFromProgID("Word.Application");
            dynamic wordApplication = Activator.CreateInstance(typeOfWordApplication);

            dynamic document = wordApplication.Documents.Add();
            dynamic paragraph = document.Content.Paragraphs.Add();
            paragraph.Range.Text = "Programowanie pod Windows";
            document.SaveAs(save_path);

            wordApplication.Quit();
        }
    }
}
