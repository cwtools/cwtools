# cwtools 	![nuget](https://img.shields.io/nuget/v/CWTools.svg)
A library for parsing, editing, and validating Paradox Interactive script files.  
Supports all modern Paradox Interactive games, and targets .net standard 2.0.

Considering contributing? [Start here!](https://github.com/tboby/cwtools/wiki/Contributing)

## Projects that use CW Tools
#### [Stellaris tech tree](http://www.draconas.co.uk/stellaristech): https://github.com/draconas1/stellaris-tech-tree
An interactive tech tree visualiser that uses CW Tools to parse the vanilla tech files, and extract localisation.
#### [SC Mod Manager](https://github.com/WojciechKrysiak/SCModManager): https://github.com/WojciechKrysiak/SCModManager/tree/feature/PortToAvalonia/PDXModLib/Utility
A mod manager that uses CW Tools for parsing and manipulating mod files.

## Example usage (C#)
This is a simple example of loading an event file, modifying it, and printing the updated events.
```csharp
            //Support UTF-8
            Encoding.RegisterProvider(CodePagesEncodingProvider.Instance);

            //Parse event file
            var text2 = File.ReadAllText("./testevent2.txt");
            var parsed2 = CWTools.CSharp.Parsers.ParseScriptFile("testevent2.txt", text2);

            var eventFile2 = parsed2.GetError();
            Console.WriteLine(eventFile2.ErrorMessage);

            //Parse file
            var text = File.ReadAllText("./testevent.txt");
            var parsed = CWTools.CSharp.Parsers.ParseScriptFile("testevent.txt", text);

            var eventFile = parsed.GetResult();

            //"Process" result into nicer format
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
```
Example from [here](https://github.com/cwtools/cwtools/blob/master/CWToolsCSTests/Program.cs)
