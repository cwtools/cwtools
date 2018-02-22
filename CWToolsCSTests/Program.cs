using System;
using CWTools.Parser;
using System.Text;
using Microsoft.FSharp.Collections;
using System.Collections.Generic;
using System.Linq;
using CWTools.Process;
using Microsoft.FSharp.Core;
using System.IO;

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
            var eventFile = CWTools.Parser.CKParser.getSuccess(parsed);

            //"Process" result into nicer format
            var processed = CK2Process.processEventFile(eventFile);

            //Find interesting event
            var myEvent = processed.Events.FirstOrDefault(x => x.ID == "test.1");
            
            //Add is_triggered_only = true
            myEvent.All = ListModule.OfSeq(myEvent.All.Append(Both.NewLeafI(new Leaf(KeyValueItem.NewKeyValueItem(Key.NewKey("is_triggered_only"), Value.NewBool(true)), Position.Empty))));

            //Output
            var output = processed.ToRaw;
            Console.WriteLine(CKPrinter.printKeyValueList(output, 0));
            PrintfModule
                .PrintFormatLine(
                    new PrintfFormat<FSharpFunc<FSharpList<Statement>, Unit>, TextWriter, Unit, Unit, FSharpList<Statement>>("%A"))
                .Invoke(output);


            //Microsoft.FSharp.Core.PrintfModule.PrintFormatLine(new PrintfFormat<T, System.IO.TextWriter, Unit, Unit>())
            //Console.WriteLine(processed.ToString());
        }
    }
}
