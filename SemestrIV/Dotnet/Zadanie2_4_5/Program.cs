using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml;
using System.Xml.Serialization;

namespace Zadanie2_4_5 {
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
        public static Student[] readArrayOfStudents(string input) {
            string xmlString = File.ReadAllText(input);
            XmlDocument document = new XmlDocument();
            document.LoadXml(xmlString);

            List<Student> listOfStudents = new List<Student>();

            foreach (XmlNode student in document.SelectNodes("//Student")) {
                Student newStudent = new Student() {
                    Name = student.Attributes["Name"].Value,
                    Surname = student.Attributes["Surname"].Value,
                    Email = student.Attributes["Email"].Value
                };

                List<Address> addresses = new List<Address>();
                foreach(XmlNode address in student.SelectNodes("Address")) {
                    Address newAddress = new Address() {
                        Type = address.Attributes["Type"].Value,
                        City = address.Attributes["City"].Value,
                        ZipCode = address.Attributes["ZipCode"].Value
                    };

                    addresses.Add(newAddress);
                }
                newStudent.Address = addresses.ToArray();

                var classes = student.SelectNodes("Class");
                List<Class> listOfClasses = new List<Class>();
                foreach(XmlNode Class in classes[0].SelectNodes("Class")) {
                    Class newClass = new Class() {
                        Name = Class.Attributes["Name"].Value,
                        Grade = int.Parse(Class.Attributes["Grade"].Value)
                    };
                    listOfClasses.Add(newClass);
                }
                newStudent.Class = listOfClasses.ToArray();

                listOfStudents.Add(newStudent);
            }

            return listOfStudents.ToArray();
        }

        public static void saveArrayOfStudents(string output, Student[] arrayOfStudents) {
            XmlDocument document = new XmlDocument();
            XmlElement arrayOfStudentsElem = document.CreateElement("ArrayOfStudent");

            foreach(Student student in arrayOfStudents) {
                XmlElement studentElem = document.CreateElement("Student");
                studentElem.SetAttribute("Name", student.Name);
                studentElem.SetAttribute("Surname", student.Surname);
                studentElem.SetAttribute("Email", student.Email);

                foreach(Address address in student.Address) {
                    XmlElement addressElem = document.CreateElement("Address");
                    addressElem.SetAttribute("Type", address.Type);
                    addressElem.SetAttribute("City", address.City);
                    addressElem.SetAttribute("ZipCode", address.ZipCode);
                    studentElem.AppendChild(addressElem);
                }

                XmlElement groupClassElem = document.CreateElement("Class");

                foreach(Class cl in student.Class) {
                    XmlElement classElem = document.CreateElement("Class");
                    classElem.SetAttribute("Name", cl.Name);
                    classElem.SetAttribute("Grade", cl.Grade.ToString());
                    groupClassElem.AppendChild(classElem);
                }

                studentElem.AppendChild(groupClassElem);

                arrayOfStudentsElem.AppendChild(studentElem);
            }

            document.AppendChild(arrayOfStudentsElem);

            using (FileStream fstream = new FileStream(output, FileMode.Create))
            using (StreamWriter writer = new StreamWriter(fstream, Encoding.UTF8)) {
                document.Save(writer);
            }
        }
    }

    class Program {
        static void Main(string[] args) {

            Student[] result = StudentXml.readArrayOfStudents("students.xml");

            foreach(Student s in result) {
                Console.WriteLine(s.Name);
            }

            StudentXml.saveArrayOfStudents("output.xml", result);

        }
    }
}
