using System;
using CWTools.Parser;
using System.Collections.Generic;
using System.Linq;
using CWTools.Process;
using Microsoft.FSharp.Core;
using System.IO;
using CWTools.CSharp;
using static CWTools.Parser.Types;
using static CWTools.Utilities.Position;

namespace CWToolsCSTests
{
    public class MyValue {
        public string Value {get; set;}
    }
    public class MyKeyValue {
        public string Key {get;set;}
        public string Value {get;set;}
    }
    public class MyNode {
        public string Key {get; set;}
        public List<MyKeyValue> KeyValues {get; set;}
        public List<MyNode> Nodes {get; set;}
        public List<MyValue> Values {get; set;}
    }

    public static class MappingSample {
        public static MyValue ToMyValue(LeafValue lv) {
            return new MyValue { Value = lv.Key };
        }

        public static MyKeyValue ToMyKeyValue(Leaf l) {
            return new MyKeyValue { Key = l.Key, Value = l.Value.ToRawString() };
        }

        public static MyNode ToMyNode(Node n) {
            // var nodes = n.AllChildren.Where(x => x.IsNodeC).Select(x => ToMyNode(x.node)).ToList();
            // var leaves = n.AllChildren.Where(x => x.IsLeafC).Select(x => ToMyKeyValue(x.leaf)).ToList();
            // var values = n.AllChildren.Where(x => x.IsLeafValueC).Select(x => ToMyValue(x.leafvalue)).ToList();
            return new MyNode { };
        }

        public static MyNode MapToMyNode() {
            //Parse event file
            var parsed = CWTools.Parser.CKParser.parseEventFile("./testevent.txt");

            var eventFile = parsed.GetResult();

            //"Process" result into nicer format
            var processed = CK2Process.processEventFile(eventFile);

            return ToMyNode(processed);
        }
    }
    class Program
    {
        static void main(string[] args)
        {
            //Parse event file
            // var parsed = CWTools.Parser.CKParser.parseEventFile("./testevent.txt");
            var text2 = File.ReadAllText("./testevent2.txt");
            var parsed2 = CWTools.CSharp.Parsers.ParseScriptFile("testevent2.txt", text2);

            var eventFile2 = parsed2.GetError();
            Console.WriteLine(eventFile2.ErrorMessage);


            //Parse event file
            // var parsed = CWTools.Parser.CKParser.parseEventFile("./testevent.txt");
            var text = File.ReadAllText("./testevent.txt");
            var parsed = CWTools.CSharp.Parsers.ParseScriptFile("testevent.txt", text);

            var eventFile = parsed.GetResult();

            //"Process" result into nicer format
            // var processed = CK2Process.processEventFile(eventFile);
            var processed = CWTools.CSharp.Parsers.ProcessStatements("testevent.txt", "./testevent.txt", eventFile);

            //Find interesting event
            var myEvent = processed.Nodes.FirstOrDefault(x => x.TagText("id") == "test.1");

            //Add is_triggered_only = true
            var leaf = new Leaf(KeyValueItem.NewKeyValueItem(Key.NewKey("is_triggered_only"), Value.NewBool(true), Operator.Equals), FSharpOption<range>.None);
            myEvent.SetTag(leaf.Key, Child.NewLeafC(leaf));
            // or
            // var newChildren = myEvent.AllChildren;
            // newChildren.Add(Leaf.Create(KeyValueItem.NewKeyValueItem(Key.NewKey("is_triggered_only"), Value.NewBool(true), Operator.Equals), range.Zero));
            // myEvent.AllChildren = newChildren;

            //Output
            var output = processed.ToRaw;
            Console.WriteLine(CKPrinter.api.prettyPrintStatement.Invoke(output));
            Console.WriteLine(output.PrettyPrint());
            PrintfModule
                .PrintFormatLine(
                    new PrintfFormat<FSharpFunc<Statement, Unit>, TextWriter, Unit, Unit, Statement>("%A"))
                .Invoke(output);

            var test = processed.Nodes.FirstOrDefault().ToRaw;
            Console.WriteLine(CKPrinter.api.prettyPrintStatement.Invoke(test));

            var files = Directory.EnumerateFiles(@"C:\Users\Thomas\Git\cwtools-stellaris-config", "*", SearchOption.AllDirectories)
                                    .Where(f => Path.GetExtension(f) == ".cwt" || Path.GetExtension(f) == ".log")
                                    .Select(f => Tuple.Create(f, File.ReadAllText(f)));

            var (rules, types, enums, complexenums, values) = CWTools.CSharp.Helpers.LoadAndInitializeFromConfigFiles(files, CWTools.Common.Game.STL, false, false);
            var typeInfo = Helpers.GetTypesInFile("events/testevent.txt", processed, types);
            PrintfModule
                .PrintFormatLine(
                    new PrintfFormat<FSharpFunc<IReadOnlyDictionary<string, IReadOnlyCollection<CWTools.Common.NewScope.TypeDefInfo>>, Unit>, TextWriter, Unit, Unit, IReadOnlyDictionary<string, IReadOnlyCollection<CWTools.Common.NewScope.TypeDefInfo>>>("%A"))
                .Invoke(typeInfo);
            Console.WriteLine(typeInfo.ToString());

        }
    }
}
