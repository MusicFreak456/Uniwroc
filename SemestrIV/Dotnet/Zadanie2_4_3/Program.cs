using System;
using System.Collections.Generic;
using System.Xml;
using System.Xml.Schema;

namespace Zadanie2_4_3 {
    class Program {
        static void Main(string[] args) {
            ValidationEventHandler eventHandler = new ValidationEventHandler(ValidationEventHandler);
            
            XmlReaderSettings settings1 = new XmlReaderSettings();
            settings1.ValidationType = ValidationType.Schema;
            Console.WriteLine("Poprawny dokument:");
            using (XmlReader validReader = XmlReader.Create("students.xml", settings1)) {
                XmlDocument validDocument = new XmlDocument();
                validDocument.Load(validReader);
                validDocument.Schemas.Add("", "students_scheme.xsd");
                validDocument.Validate(eventHandler);
            }

            Console.WriteLine("Niepoprawny dokoment:");
            XmlReaderSettings settings2 = new XmlReaderSettings();
            settings2.ValidationType = ValidationType.Schema;
            using (XmlReader invalidReader = XmlReader.Create("bad_students.xml", settings2)) {
                XmlDocument invalidDocument = new XmlDocument();
                invalidDocument.Load(invalidReader);
                invalidDocument.Schemas.Add("", "students_scheme.xsd");
                invalidDocument.Validate(eventHandler);
            }
        }

        static void ValidationEventHandler(object sender, ValidationEventArgs args) {
            switch (args.Severity) {
                case XmlSeverityType.Error:
                    Console.WriteLine("Błąd: {0}", args.Message);
                    break;
                case XmlSeverityType.Warning:
                    Console.WriteLine("Ostrzeżenie: {0}", args.Message);
                    break;
            }
        }
    }
}
