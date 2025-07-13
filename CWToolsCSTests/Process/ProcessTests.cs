using System;
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
        _root = Parsers.ProcessStatements(
            "test",
            "",
            Parsers.ParseScriptFile("", Text).GetResult()
        );
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
        Assert.That(comments[0].Comment, Is.EqualTo(" comment1"));
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
        var common = (Child.CommentC)children[0];
        var leaf1 = (Child.LeafC)children[1];
        var leaf2 = (Child.LeafC)children[2];
        var leaf3 = (Child.LeafC)children[3];
        var node = (Child.NodeC)children[4];
        
        Assert.That(children[0].IsCommentC, Is.True);
        Assert.That(children[1].IsLeafC, Is.True);
        Assert.That(children[2].IsLeafC, Is.True);
        Assert.That(children[3].IsLeafC, Is.True);
        Assert.That(children[4].IsNodeC, Is.True);
        using (Assert.EnterMultipleScope())
        {
            Assert.That(common.comment.Comment, Is.EqualTo(" comment1"));
            Assert.That(leaf1.leaf.Key, Is.EqualTo("key1"));
            Assert.That(leaf2.leaf.Key, Is.EqualTo("key2"));
            Assert.That(leaf3.leaf.Key, Is.EqualTo("key3"));
            Assert.That(node.node.Key, Is.EqualTo("node1"));
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

    [Test]
    public void ToRawTest()
    {
        Assert.That(
            _root.ToRaw.PrettyPrint(),
            Is.EqualTo(
                "test = {\n\t# comment1\n\tkey1 = value1\n\tkey2 = \"value2\"\n\tkey3 > 1\n\tnode1 = {\n\t\tkey2 = value2\n\t}\n}\n"
            )
        );
    }
}
