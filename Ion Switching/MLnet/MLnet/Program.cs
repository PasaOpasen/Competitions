using System;
using System.IO;

namespace MLnet
{
    class Program
    {
        static void Main(string[] args)
        {
            int k = 0;

            double conv(string val) => Convert.ToDouble(val.Replace('.', ','));

            using (StreamWriter writer = new StreamWriter(@"C:\Users\qtckp\OneDrive\Рабочий стол\Competitions\Ion Switching\train for MLnet.tsv"))
                foreach (var line in File.ReadAllLines(@"C:\Users\qtckp\OneDrive\Рабочий стол\Competitions\Ion Switching\train for py.csv"))
                {
                    if(k==0)
                        writer.WriteLine(line.Replace(",batches","").Replace(',', '\t').Replace('.', ','));
                    else if(k%8==0)
                    {
                    var arr = line.Split(',');
                    writer.WriteLine($"{conv(arr[0])}\t{conv(arr[1])}\t{conv(arr[3])}\t{conv(arr[4])}\t{conv(arr[5])}\t{conv(arr[6])}\t{conv(arr[7])}\t{conv(arr[8])}\t{conv(arr[9])}\t{conv(arr[10])}");
                    }
                    k++;


                    //writer.WriteLine(line.Replace(',', '\t').Replace('.', ','));
                    //k++;
                    //if (k == 1489410)
                    //    Console.WriteLine(line);
                }
                        
        }
    }
}
