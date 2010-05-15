using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System;
using System.IO;
using Trinet.Networking;

namespace shares
{
  class Program
  {
    [STAThread]
    static int Main(string[] args)
    {
      return TestShares(args[0]);
    }
	
    static int TestShares(string server) 
    {
      ShareCollection shi;
      if (server != null && server.Trim().Length > 0) {
        shi = ShareCollection.GetShares(server);
        if (shi != null) {
          foreach(Share si in shi) {
            if (si.IsFileSystem) {
              Console.WriteLine("{0}", si);
            }
          }
        }
        else
          return -1;
      }
      return 0;
    }
  }
}
