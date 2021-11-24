using System;
using System.Text;
using System.Runtime.InteropServices;

namespace Zadanie1_5_1 {
    class Program {
        // Na podstawie dokumentacji i pinvoke.net

        [DllImport("user32.dll", CharSet = CharSet.Unicode, SetLastError = true)]
        private static extern int MessageBox(IntPtr hWnd, string lpText, string lpCaption, uint uType);

        [DllImport("advapi32.dll", SetLastError = true)]
        private static extern bool GetUserName(StringBuilder lpBuffer, ref Int32 pcbBuffer);

        static void Main(string[] args) {
            Int32 pcbBuffer = 64;
            StringBuilder lpBuffer = new StringBuilder(pcbBuffer);
            GetUserName(lpBuffer, ref pcbBuffer);
            MessageBox(IntPtr.Zero, lpBuffer.ToString(), "Nazwa użytkownika", 0);
        }
    }
}
