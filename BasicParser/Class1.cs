using LanguageExt;
using static LanguageExt.Prelude;
using Microsoft.CodeAnalysis;
using  Microsoft.CodeAnalysis.CSharp;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using Irony.Parsing;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace BasicParser
{
    [TestFixture]
    public class Class1
    {

        Parser CreateParser()
        {
            //var grammar = new TerminalTestGrammar(terminal, terminator);
            var grammar = new GWBasicGrammar();
            var parser = new Parser(grammar);
            CheckGrammarErrors(parser);
            return parser;
        }

        public static void CheckGrammarErrors(Parser parser)
        {
            var errors = parser.Language.Errors;
            if (errors.Count > 0)
                throw new Exception("Unexpected grammar contains error(s): " + string.Join("\n", errors));
        }

        Either<Irony.LogMessageList, ParseTree> ParseBasic(string sourceText)
        {
            var parser = CreateParser();
            var tree = parser.Parse(sourceText);

            switch (tree.Status)
            {
                case ParseTreeStatus.Error:
                    throw new Exception(ParserMessagesToString(tree.ParserMessages));
                    return Left(tree.ParserMessages);
                case ParseTreeStatus.Parsed:
                    return Right(tree);
                case ParseTreeStatus.Parsing:
                    return Left(tree.ParserMessages);
                case ParseTreeStatus.Partial:
                    return Left(tree.ParserMessages);
                default:
                    throw new Exception("Cannot happen");
            }
        }

        string ParserMessagesToString(Irony.LogMessageList msgs)
        {
            return msgs.Select(msg => $"{msg.Message}. {msg.Location}")
                .Reduce((a, b) => $"{a} {b}");
        }

        //[Test]
        //public void ParseIfThatFails()
        //{
        //    var stmt = @"
        //        IF SEND.TO.SUPERUSER THEN
        //            USR.ID = 'SUPERUSER'
        //            GOSUB SEND.MSG
        //        END";

        //    var res = ParseBasic(stmt);

        //    Assert.AreEqual(true, res.IsLeft);
        //}

        //IEnumerable<ParseTreeNode> WalkChildren(ParseTreeNode node) =>
        //    node.ChildNodes.Map(WalkTree);


        //SyntaxKind
        //    ExpressionSyntax
        //    StatementSyntax

        public class Syntax
        {
            public Syntax(SyntaxType notificationType)
            {
                NotificationType = notificationType;
            }

            public SyntaxType NotificationType { get; }
        }

        public enum SyntaxType
        {
            SyntaxKind,
            ExpressionSyntax,
            StatementSyntax,
            StatementListSyntax
        }

        public class Kind : Syntax
        {
            public Kind(SyntaxKind kind) : base(SyntaxType.SyntaxKind)
            {
                value = kind;
            }
            public SyntaxKind value { get; }
        }

        public class Expression : Syntax
        {
            public Expression(ExpressionSyntax expr) : base(SyntaxType.ExpressionSyntax)
            {
                value = expr;
            }
            public ExpressionSyntax value { get; }
        }

        public class Statement : Syntax
        {
            public Statement(StatementSyntax stmt) : base(SyntaxType.StatementSyntax)
            {
                value = stmt;
            }
            public StatementSyntax value { get; }
        }

        //public class StatementList : Syntax
        //{
        //    public StatementList(SingletonList<StatementSyntax> stmt) : base(SyntaxType.StatementListSyntax)
        //    {
        //        value = stmt;
        //    }
        //    public SingletonList<StatementSyntax> value { get; }
        //}

        public void UseSyntax(Syntax syntax)
        {
            switch (syntax.NotificationType)
            {
               // case SyntaxType.StatementSyntax:
                   // (Statement)syntax;

            }
        }




        Syntax WalkTree(ParseTreeNode node)
        {
            switch(node.Term.Name)
            {
                case "PROGRAM":
                case "STATEMENT":
                case "THEN_CLAUSE":
                case "END_STMT":
                    //WalkChildren(node);
                    node.ChildNodes.ForEach(n => WalkTree(n));
                    break;
                case "STATEMENT_LIST":
                    return ToStmtList(node);
                case "IF_STMT":
                    return ToIfStmt(node);
                case "EXPRESSION":
                    if (node.ChildNodes.Count == 1)
                    {
                        return WalkTree(node.ChildNodes[0]);
                    }
                    else
                    {
                        throw new Exception("Expression has more than one child");
                    }
                case "BINARY_EXPR":
                    return ToBinaryExpression(node);
                //case "BINARY_OP":
                //    return ToBinaryOp(node);
                //    break;
                case "NUMBER":
                    return ToNumber(node);
                case "if":
                case "then":
                case "end":
                    //ignore
                    break;
                default:
                    throw new Exception($"Node Term not handled: {node.Term.Name}");
            }
            
            return null;
        }

        Syntax ToBinaryExpression(ParseTreeNode node)
        {
            var one = (WalkTree(node.ChildNodes[0]) as Expression).value;
            var binary_op = (ToBinaryOp(node.ChildNodes[1]) as Kind).value;
            var two = (WalkTree(node.ChildNodes[2]) as Expression).value;
            
            return new Expression(BinaryExpression(binary_op, one, two));
        }

        Syntax ToBinaryOp(ParseTreeNode node)
        {
            switch (node.ChildNodes[0].Token.ValueString)
            {
                case "=":
                    return new Kind(SyntaxKind.EqualsExpression);
                default:
                    throw new Exception($"Binary Operator not handled: {node.Term.Name}");
            }
        }


        Syntax ToNumber(ParseTreeNode node)
        {
            return new Expression(
                LiteralExpression(
                    SyntaxKind.NumericLiteralExpression,
                    Literal(Int32.Parse(node.Token.ValueString))));
        }

        Syntax ToStmtList(ParseTreeNode node)
        {
            //var l = node.ChildNodes
            //    .Map(WalkTree)
            //    .Fold(new SyntaxList<StatementSyntax>(), (a, b) => a.Add(b))
            //    .ToList();
            
            return null;
            //var l = SyntaxList<StatementSyntax>(l);
            //return new Statement(
            //    SingletonList<StatementSyntax>());
        }

        StatementSyntax ToStmt(ParseTreeNode node)
        {

            return null;
        }

        Syntax ToIfStmt(ParseTreeNode node)
        {
            ExpressionSyntax cond = (WalkTree(node.ChildNodes[1]) as Expression).value;
            StatementSyntax then = (ToThenStmt(node.ChildNodes[2]) as Statement).value;
            
            return new Statement(IfStatement(cond, then));
        }

        Syntax ToThenStmt(ParseTreeNode node)
        {
            //StatementSyntax stmtList = (WalkTree(node.ChildNodes[1]) as Statement).value;
            Syntax stmtList = ToStmtList(node.ChildNodes[1]);
            //Block(
            //   SingletonList<StatementSyntax>(
            //       ReturnStatement(
            //           LiteralExpression(
            //               SyntaxKind.TrueLiteralExpression)
            
            var b = Block();
            return null;//new Statement(Block(stmtList));
        }

        [Test]
        public void RunWalkTree()
        {
            var stmt = @"IF 1 = 1 THEN 1 END";
            var res = ParseBasic(stmt);
            res.IfRight(t => WalkTree(t.Root));
        }


        [Test]
        public void ParseSimpleIf()
        {
            var stmt = @"IF 1 THEN 1 END";

            var res = ParseBasic(stmt);

            Assert.AreEqual(true, res.IsRight);
            res.IfRight(tree => {
                Assert.AreNotEqual(null, tree.Root);
                });
        }

        [Test]
        public void ParseSimpleIfWLineBreak()
        {
            var stmt =
@"IF 1 THEN
   1
END";

            var res = ParseBasic(stmt);

            Assert.AreEqual(true, res.IsRight);
        }

        //        [Test]
        //        public void ParseSimpleIfWTwoStatements()
        //        {
        //            var stmt =
        //@"IF 1 THEN
        //   1
        //   1
        //END";

        //            var res = ParseBasic(stmt);

        //            Assert.AreEqual(true, res.IsRight);
        //        }

        [Test]
        public void ParseSimpleIfWExpression()
        {
            var stmt = @"IF 1 = 1 THEN 1 END";

            var res = ParseBasic(stmt);

            Assert.AreEqual(true, res.IsRight);
            res.IfRight(tree => {
                Assert.AreNotEqual(null, tree.Root);
            });
        }





    }
}
