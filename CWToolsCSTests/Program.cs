using System;
using CWTools.Parser;
using System.Text;
using Microsoft.FSharp.Collections;
using System.Collections.Generic;
using System.Linq;
using CWTools.Process;
using Microsoft.FSharp.Core;
using System.IO;
using CWTools.CSharp;

namespace CWToolsCSTests
{
    class Program
    {
        static void Main(string[] args)
        {
            //Support UTF-8
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

            //Parse event file
            var parsed = CWTools.Parser.CKParser.parseEventFile("./testevent.txt");
            
            var eventFile = parsed.GetResult();

            //"Process" result into nicer format
            var processed = CK2Process.processEventFile(eventFile);

            //Find interesting event
            var myEvent = processed.Events.FirstOrDefault(x => x.ID == "test.1");
            
            //Add is_triggered_only = true
            var leaf = new Leaf("is_triggered_only", Value.NewBool(true));
            myEvent.AllChildren.Add(Child.NewLeafC(leaf));
            // or
            // myEvent.AllChildren.Add(Leaf.Create("is_triggered_only", Value.NewBool(true)));

            //Output
            var output = processed.ToRaw;
            Console.WriteLine(CKPrinter.printKeyValueList(output, 0));
            PrintfModule
                .PrintFormatLine(
                    new PrintfFormat<FSharpFunc<FSharpList<Statement>, Unit>, TextWriter, Unit, Unit, FSharpList<Statement>>("%A"))
                .Invoke(output);
        }
    }
}
