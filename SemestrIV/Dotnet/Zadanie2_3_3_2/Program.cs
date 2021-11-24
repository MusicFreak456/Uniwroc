using System;
using System.IO;
using System.IO.Compression;
using System.Security.Cryptography;
using System.Text;

namespace Zadanie2_3_3_2 {
    class Program {
        static void Main(string[] args) {
            byte[] key = Encoding.UTF8.GetBytes("secretsecretsecretsecretsecretse"); // Dla ułatwienia, wiadomo że to nie powinno tak wyglądać
            byte[] IV = new byte[16];

            using (FileStream iFileStream = new FileStream("output.gz", FileMode.Open, FileAccess.Read))
            using (GZipStream zipStream = new GZipStream(iFileStream, CompressionMode.Decompress))
            using (AesManaged aesAlg = new AesManaged())
            using (CryptoStream cryStream = new CryptoStream(zipStream, aesAlg.CreateDecryptor(key, IV), CryptoStreamMode.Read))
            using (StreamReader streamReader = new StreamReader(cryStream)) {
                foreach (var character in streamReader.ReadToEnd()) {
                    Console.Write(character);
                }
            }
        }
    }
}
