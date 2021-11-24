using System;
using System.IO;
using System.Text;
using System.Xml.Serialization;

namespace Zadanie2_4_4 {
    public class Student {
        [XmlAttribute]
        public string Name { get; set; }
        [XmlAttribute]
        public string Surname { get; set; }
        [XmlAttribute]
        public string Email { get; set; }
        [XmlElement]
        public Address[] Address { get; set; }
        [XmlArray]
        public Class[] Class { get; set; }
    }   

    public class Address {
        [XmlAttribute]
        public string Type { get; set; }
        [XmlAttribute]
        public string City { get; set; }
        [XmlAttribute]
        public string ZipCode { get; set; }
    }

    public class Class {
        [XmlAttribute]
        public string Name { get; set; }
        [XmlAttribute]
        public int Grade { get; set; }
    }

    public static class StudentXml {
        public static void SerializeArrayOfStudents(string output,Student[] arrayOfStudents) {
            using (FileStream fileStream = new FileStream(output, FileMode.Create))
            using (StreamWriter writer = new StreamWriter(fileStream, Encoding.UTF8)) {
                XmlSerializer serializer = new XmlSerializer(typeof(Student[]));
                serializer.Serialize(writer, arrayOfStudents);
            }
        }

        public static Student[] DeserializeArrayOfStudents(string input) {
            using (FileStream fileStream = new FileStream(input, FileMode.Open))
            using (StreamReader reader = new StreamReader(fileStream)) {
                XmlSerializer serializer = new XmlSerializer(typeof(Student[]));
                return (Student[]) serializer.Deserialize(reader);
            }
        }
    }

    class Program {
        static void Main(string[] args) {
            Student student1 = new Student {
                Name = "Jan",
                Surname = "Kowalski",
                Email = "example@example.com",
                Address = new Address[] {
                    new Address() {
                        Type = "Permanent",
                        City = "Somewhere",
                        ZipCode = "10-100"
                    },
                    new Address() {
                        Type = "Temporary",
                        City = "SomewhereElse",
                        ZipCode = "01-001"
                    }
                },
                Class = new Class[] {
                    new Class() {
                        Name = "Programowanie pod Windows .NET 2021",
                        Grade = 5
                    }
                }
            };

            Student student2 = new Student {
                Name = "Maksymilian",
                Surname = "Debaściak",
                Email = "example@example.com",
                Address = new Address[] {
                    new Address() {
                        Type = "Permanent",
                        City = "Somewhere",
                        ZipCode = "10-100"
                    },
                    new Address() {
                        Type = "Temporary",
                        City = "SomewhereElse",
                        ZipCode = "01-001"
                    }
                },
                Class = new Class[] {
                    new Class() {
                        Name = "Rachunek prawdopodobieństwa i statystyka",
                        Grade = 5
                    },
                    new Class() {
                        Name = "Sieci komputerowe",
                        Grade = 5
                    }
                }
            };

            Student[] arrayOfStudents = new Student[] { student1, student2 };

            StudentXml.SerializeArrayOfStudents("output.xml",arrayOfStudents);

            Student[] result = StudentXml.DeserializeArrayOfStudents("output.xml");

            foreach(var student in result) {
                Console.WriteLine(student.Surname);
            }
        }
    }
}
