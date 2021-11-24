using System;
using System.Reflection;
using System.IO;

// Na podstawie: https://www.youtube.com/watch?v=_61pLVH2qPk

namespace Zadanie2_3_10 {

    public class EmbededResource {

        private string nameSpace;
        public EmbededResource(string nameSpace) {
            this.nameSpace = nameSpace;
        }

        public void ExtractToFile(string resourceFilePath, string destinationFilePath) {
            Assembly assembly = Assembly.GetCallingAssembly();

            using (Stream iStream = assembly.GetManifestResourceStream(this.nameSpace + "." + resourceFilePath) )
            using (BinaryReader binaryReader = new BinaryReader(iStream))
            using (FileStream oFileStream = new FileStream(destinationFilePath, FileMode.Create))
            using (BinaryWriter binaryWriter = new BinaryWriter(oFileStream)) {
                binaryWriter.Write(binaryReader.ReadBytes((int)iStream.Length));
            }
        }
    }

    class Program {

        static void Main(string[] args) {
            EmbededResource embededResource = new EmbededResource("Zadanie2_3_10");
            embededResource.ExtractToFile("Resources.EmbededTest.txt", "EmbededTest.txt");
            
        }
    }
}
