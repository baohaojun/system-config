using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Diagnostics;


namespace ConsoleApplication1
{
  class Program
  {
    static void Main(string[] args)
    {
      Process[] processes = Process.GetProcesses();

      foreach (Process p in processes) {
        try {
          foreach (ProcessModule module in p.Modules) {
            Console.WriteLine("({0}) ({1}) : {2}", p.ProcessName, p.Id, module.FileName);
          }
        } catch {
          Console.WriteLine("Process id {0:D} exception", p.Id);
        }
      }
    }
  }
}
