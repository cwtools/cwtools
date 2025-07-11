using System.Linq;
using CWTools.CSharp;
using CWTools.Parser;
using CWTools.Process;
using CWTools.Utilities;
using NUnit.Framework;

namespace CWToolsCSTests.Process;

public sealed class ProcessTests
{
    private const string Text = """
        # comment1
        key1 = value1
        key2 = "value2"
        key3 > 1
        node1 = {
            key2 = value2
        }
        """;

    private Node _root;

    [SetUp]
    public void Setup()
    {
        _root = Parsers.ProcessStatements("", "", Parsers.ParseScriptFile("", Text).GetResult());
    }

    [Test]
    public void LeavesTest()
    {
        Assert.That(_root.Leaves.ToArray(), Has.Length.EqualTo(3));
    }

    [Test]
    public void CommentsTest()
    {
        var comments = _root.Comments.ToArray();

        Assert.That(comments, Has.Length.EqualTo(1));
        Assert.That(comments[0].Item2, Is.EqualTo(" comment1"));
    }

    [Test]
    public void NodesTest()
    {
        Assert.That(_root.Nodes.ToArray(), Has.Length.EqualTo(1));
    }

    [Test]
    public void LeafParseResultTest()
    {
        var key1 = _root.Leaves.First(leaf => leaf.Key == "key1");
        var key2 = _root.Leaves.First(leaf => leaf.Key == "key2");

        Assert.That(key1, Is.Not.Null);
        Assert.That(key1.Value.ToRawString(), Is.EqualTo("value1"));
        Assert.That(key1.Value.IsString, Is.True);
        Assert.That(key2, Is.Not.Null);
        Assert.That(key2.Value.ToRawString(), Is.EqualTo("value2"));
        Assert.That(key2.Value.IsQString, Is.True);
    }

    [Test]
    public void LeafTest()
    {
        var leaf = new Leaf(
            "key1",
            Types.Value.CreateString("value"),
            Position.range.Zero,
            Types.Operator.Equals
        );
        using (Assert.EnterMultipleScope())
        {
            Assert.That(leaf.Key, Is.EqualTo("key1"));
            Assert.That(leaf.Value.ToRawString(), Is.EqualTo("value"));
            Assert.That(leaf.Operator, Is.EqualTo(Types.Operator.Equals));
            Assert.That(leaf.Value.IsString, Is.True);
        }
    }

    [Test]
    public void LeafValuesTest()
    {
        var leaf = LeafValue.Create(Types.Value.CreateString("value"));

        using (Assert.EnterMultipleScope())
        {
            Assert.That(leaf.Value.ToRawString(), Is.EqualTo("value"));
            Assert.That(leaf.Key, Is.EqualTo(leaf.Value.ToRawString()));
            Assert.That(leaf.ValueText, Is.EqualTo(leaf.Value.ToRawString()));
        }
    }

    [Test]
    public void NodeTest()
    {
        var node = new Node("key1", Position.range.Zero);

        using (Assert.EnterMultipleScope())
        {
            Assert.That(node.Key, Is.EqualTo("key1"));
            Assert.That(node.AllArray, Is.Empty);
            Assert.That(node.Leaves, Is.Empty);
            Assert.That(node.Comments, Is.Empty);
            Assert.That(node.Position, Is.EqualTo(Position.range.Zero));
        }
    }

    [Test]
    public void ChildrenOrderTest()
    {
        var children = _root.AllArray;
        var common = children[0].comment;
        var leaf1 = children[1].leaf;
        var leaf2 = children[2].leaf;
        var leaf3 = children[3].leaf;
        var node = children[4].node;

        Assert.That(children[0].IsCommentC, Is.True);
        Assert.That(children[1].IsLeafC, Is.True);
        Assert.That(children[2].IsLeafC, Is.True);
        Assert.That(children[3].IsLeafC, Is.True);
        Assert.That(children[4].IsNodeC, Is.True);
        using (Assert.EnterMultipleScope())
        {
            Assert.That(common.Item2, Is.EqualTo(" comment1"));
            Assert.That(leaf1.Key, Is.EqualTo("key1"));
            Assert.That(leaf2.Key, Is.EqualTo("key2"));
            Assert.That(leaf3.Key, Is.EqualTo("key3"));
            Assert.That(node.Key, Is.EqualTo("node1"));
        }
    }

    [Test]
    public void OperatorTest()
    {
        var key1 = _root.Leaves.First(leaf => leaf.Key == "key1");
        var key2 = _root.Leaves.First(leaf => leaf.Key == "key2");
        var key3 = _root.Leaves.First(leaf => leaf.Key == "key3");

        using (Assert.EnterMultipleScope())
        {
            Assert.That(key1.Operator, Is.EqualTo(Types.Operator.Equals));
            Assert.That(key2.Operator, Is.EqualTo(Types.Operator.Equals));
            Assert.That(key3.Operator, Is.EqualTo(Types.Operator.GreaterThan));
        }
    }
}
