using System;
public class Tool {
        public static void Main (string[] args)
        {
                Uri uri = new Uri("file:///home/bhj/system-config/gcode/playground/hello%20world,%25+++&&&shit%E4%BD%A0%E5%A5%BD");
                Console.WriteLine("{0}: {1}", uri, uri.LocalPath);

                Console.WriteLine("hello world");
        }
}
