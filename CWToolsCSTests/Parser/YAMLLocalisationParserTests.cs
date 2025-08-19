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
                                key7:"val§ue7"#comment5
                                key8:"val:ue8"#comment6
                                #comment5
                                key9:" "internal quotes " # not a comment and more" # comment7
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

    /// <summary>
    /// What the localisation parser currently does
    /// </summary>
    [Test]
    public void ParseRegressionTest()
    {
        var result = YAMLLocalisationParser.parseLocText(Text, "test.txt").GetResult();

        var key1 = result.entries.First(loc => loc.key == "key1");
        var key2 = result.entries.First(loc => loc.key == "key2");
        var key3 = result.entries.First(loc => loc.key == "key3");
        var key4 = result.entries.First(loc => loc.key == "key4");
        var key5 = result.entries.First(loc => loc.key == "key5");
        var key6 = result.entries.First(loc => loc.key == "key6");
        var key7 = result.entries.First(loc => loc.key == "key7");
        var key8 = result.entries.First(loc => loc.key == "key8");
        var key9 = result.entries.First(loc => loc.key == "key9");

        Assert.That(key1.desc, Is.EqualTo("\" value1\" #comment1"));
        Assert.That(key1.value, Is.Null);

        // Current behavior: start at the first double quote and include the rest of the line
        Assert.That(key2.desc, Is.EqualTo("\"value2\""));
        Assert.That(key2.value.Value, Is.EqualTo('2'));

        Assert.That(key3.desc, Is.EqualTo("\"\" #comment2"));

        Assert.That(key4.desc, Is.EqualTo("\"va\\\"lue4\" #comment3"));
        Assert.That(key4.value, Is.Null);

        Assert.That(key5.desc, Is.EqualTo("\"va\\\"lue5\" #comment4"));
        Assert.That(key5.value.Value, Is.EqualTo('5'));

        Assert.That(key6.desc, Is.EqualTo("\"value6\"#comment4"));
        Assert.That(key7.desc, Is.EqualTo("\"val§ue7\"#comment5"));
        Assert.That(key8.desc, Is.EqualTo("\"val:ue8\"#comment6"));
        Assert.That(key9.desc, Is.EqualTo("\" \"internal quotes \" # not a comment and more\" # comment7"));

        Assert.That(key1.position, Is.EqualTo(result.entries.First().position));
    }

    /// <summary>
    /// What we'd like it to do
    /// </summary>
    [Test]
    [Ignore("Not implemented yet")]
    public void ParseResultTest()
    {
        var result = YAMLLocalisationParser.parseLocText(Text, "test.txt").GetResult();

        var key1 = result.entries.First(loc => loc.key == "key1");
        var key2 = result.entries.First(loc => loc.key == "key2");
        var key3 = result.entries.First(loc => loc.key == "key3");
        var key4 = result.entries.First(loc => loc.key == "key4");
        var key5 = result.entries.First(loc => loc.key == "key5");
        var key6 = result.entries.First(loc => loc.key == "key6");
        var key7 = result.entries.First(loc => loc.key == "key7");
        var key8 = result.entries.First(loc => loc.key == "key8");
        var key9 = result.entries.First(loc => loc.key == "key9");

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
        Assert.That(key7.desc, Is.EqualTo("val§ue7"));
        Assert.That(key8.desc, Is.EqualTo("val:ue8"));
        Assert.That(key9.desc, Is.EqualTo(" \"internal quotes \" # not a comment and more"));
        Assert.That(key1.position, Is.EqualTo(result.entries.First().position));
    }


    [Test]
    public void ParseKeyTest()
    {
        var result = YAMLLocalisationParser.parseLocText(Text, "test.txt").GetResult();

        Assert.That(result.key, Is.EqualTo("l_simp_chinese"));
    }
}