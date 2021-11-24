using Microsoft.CSharp;
using System;
using System.CodeDom.Compiler;
using System.Reflection;
using System.Text;

namespace Zadanie2_3_11 {
    class Evaluator {
        // Na podstawie podręcznika, rozdział 2.5.3
        public static object Evaluate(string methodBody) {
            ICodeCompiler compiler = new CSharpCodeProvider().CreateCompiler();
            CompilerParameters parameters = new CompilerParameters();
            parameters.ReferencedAssemblies.Add("system.dll");
            parameters.GenerateExecutable = false;
            parameters.GenerateInMemory = true;

            StringBuilder code = new StringBuilder();
            code.Append("using System; \n");
            code.Append("namespace _Evaluator { \n");
            code.Append("    public class _Evaluator { \n");

            code.Append("        public object UserMethod(){ \n");
            code.Append(methodBody);
            code.Append("            return null;");
            code.Append("        }\n");

            code.Append("    }\n");
            code.Append("}");

            CompilerResults result = compiler.CompileAssemblyFromSource(parameters, code.ToString());
            if (result.Errors.HasErrors) {
                StringBuilder errorMessages = new StringBuilder();
                foreach(CompilerError error in result.Errors) {
                    errorMessages.Append(error + "\n");
                }
                throw new Exception("Wystąpiły błędy podczas kompilacji: \n" + errorMessages.ToString());
            }

            Assembly assembly = result.CompiledAssembly;
            var instance = assembly.CreateInstance("_Evaluator._Evaluator");
            MethodInfo method = instance.GetType().GetMethod("UserMethod");
            return method.Invoke(instance, null);
        }
    }
    class Program {
        static void Main(string[] args) {
            StringBuilder methodBody = new StringBuilder();
            string line = "";

            Console.WriteLine("Podaj ciało funkcji, która ma być wykonana:");
            Console.WriteLine("(Ostatnią linię należy zasygnalizować umieszczając " +
                "w niej znak końca przekazu (ctrl + z))\n");

            do {
                methodBody.Append(line);
                line = Console.ReadLine();
            } while (line != null);

            Console.WriteLine("\nWynik obliczenia:");
            Console.WriteLine(Evaluator.Evaluate(methodBody.ToString()));
        }
    }
}
