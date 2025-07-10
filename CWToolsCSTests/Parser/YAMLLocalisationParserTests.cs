using System.Linq;
using CWTools.CSharp;
using CWTools.Localisation;
using NUnit.Framework;

namespace CWToolsCSTests.Parser;

[TestFixture(TestOf = typeof(YAMLLocalisationParser))]
public sealed class YAMLLocalisationParserTests
{
    private const string Text = """
        #comment
        l_simp_chinese:#comment
        #comment
        key1: " value1" #comment1
        key2:2 "value2"
        key3: "" #comment2
        key4: "va\"lue4" #comment3
        key5:5 "va\"lue5" #comment4
        key6:"value6"#comment4
        #comment5
        """;

    private const string Failure = """
        l_simp_chinese:
         key1:
        """;

    [Test]
    public void ParseTest()
    {
        var result = YAMLLocalisationParser.parseLocText(Text, string.Empty);
        var failure = YAMLLocalisationParser.parseLocText(Failure, string.Empty);

        Assert.That(result.IsSuccess, Is.True);
        Assert.That(result.IsFailure, Is.False);
        Assert.That(failure.IsSuccess, Is.False);
        Assert.That(failure.IsFailure, Is.True);
    }

    [Test]
    public void ParseResultTest()
    {
        var result = YAMLLocalisationParser.parseLocText(Text, "test.txt").GetResult();

        var key1 = result.entries.First(loc => loc.key == "key1");
        var key2 = result.entries.First(loc => loc.key == "key2");
        var key3 = result.entries.First(loc => loc.key == "key3");
        var key4 = result.entries.First(loc => loc.key == "key4");
        var key5 = result.entries.First(loc => loc.key == "key5");
        var key6 = result.entries.First(loc => loc.key == "key6");

        Assert.That(key1.desc, Is.EqualTo(" value1"));
        Assert.That(key1.value, Is.Null);
        Assert.That(key2.desc, Is.EqualTo("value2"));
        Assert.That(key2.value.Value, Is.EqualTo('2'));
        Assert.That(key3.desc, Is.EqualTo(""));
        Assert.That(key4.desc, Is.EqualTo("va\"lue4"));
        Assert.That(key4.value, Is.Null);
        Assert.That(key5.desc, Is.EqualTo("va\"lue5"));
        Assert.That(key5.value.Value, Is.EqualTo('5'));
        Assert.That(key6.desc, Is.EqualTo("value6"));
        Assert.That(key1.position, Is.EqualTo(result.entries.First().position));
    }

    [Test]
    public void ParseKeyTest()
    {
        var result = YAMLLocalisationParser.parseLocText(Text, "test.txt").GetResult();

        Assert.That(result.key, Is.EqualTo("l_simp_chinese"));
    }
}
