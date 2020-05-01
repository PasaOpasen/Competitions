using System;
using System.IO;

namespace MLnet
{
    class Program
    {
        static void Main(string[] args)
        {
            int k = 0;

            using (StreamWriter writer = new StreamWriter(@"C:\Users\qtckp\OneDrive\Рабочий стол\Competitions\Ion Switching\train for MLnet.tsv"))
                foreach (var line in File.ReadAllLines(@"C:\Users\qtckp\OneDrive\Рабочий стол\Competitions\Ion Switching\train for py.csv"))
                {
                  writer.WriteLine(line.Replace(',', '\t').Replace('.', ','));
                    k++;
                    if (k == 1489410)
                        Console.WriteLine(line);
                }
                        
        }
    }
}
