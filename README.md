# cwtools 	![nuget](https://img.shields.io/nuget/v/CWTools.svg)
A library for parsing, editing, and validating Paradox Interactive script files.  
Supports all modern Paradox Interactive games, and targets .net standard 2.0.

## Example usage (C#)
This is a simple example of loading an event file, modifying it, and printing the updated events.
```csharp
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
            myEvent.AllChildren.Add(Leaf.Create("is_triggered_only", Value.NewBool(true)));

            //Output
            var output = processed.ToRaw;
            Console.WriteLine(CKPrinter.printKeyValueList(output, 0));
```
Which will take a file like
```
namespace = test

#One event
country_event = {
        id = test.1
    desc = "test description"
}
#Another event
country_event = {
    id = test.2
desc = "test 2 description"
}
```
and output a file like
```
namespace = test
#One event
country_event = {
        is_triggered_only = yes
        id = test.1
        desc = "test description"
         }
#Another event
country_event = {
        id = test.2
        desc = "test 2 description"
         }
```
