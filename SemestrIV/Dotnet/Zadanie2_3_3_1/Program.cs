using System;
using System.IO;
using System.IO.Compression;
using System.Security.Cryptography;
using System.Text;

namespace Zadanie2_3_3_1 {
    class Program {
        static void Main(string[] args) {
            byte[] key = Encoding.UTF8.GetBytes("secretsecretsecretsecretsecretse"); // Dla ułatwienia, wiadomo że to nie powinno tak wyglądać
            byte[] IV = new byte[16];

            Console.WriteLine("Podaj ścieżkę do pliku, który chcesz zakodować");
            string path = Console.ReadLine();

            using (FileStream iFileStream = new FileStream(path, FileMode.Open, FileAccess.Read))
            using (StreamReader streamReader = new StreamReader(iFileStream))

            using (FileStream oFileStream = new FileStream("output.gz", FileMode.Create))
            using (GZipStream zipStream = new GZipStream(oFileStream, CompressionLevel.Optimal))
            using (AesManaged aesAlg = new AesManaged())
            using (CryptoStream cryStream = new CryptoStream(zipStream, aesAlg.CreateEncryptor(key, IV), CryptoStreamMode.Write))
            using (StreamWriter streamWriter = new StreamWriter(cryStream)) {
                foreach (var character in streamReader.ReadToEnd()) {
                    streamWriter.Write(character);
                }
            }

            Console.WriteLine("Wynik został umieszczony w pliku output.gz");
        }
    }
}
