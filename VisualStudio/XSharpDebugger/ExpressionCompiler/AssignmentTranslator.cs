// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using IrisCompiler;
using IrisCompiler.FrontEnd;
using System.IO;
using System.Text;

namespace XSharpDebugger.ExpressionCompiler
{
    /// <summary>
    /// A subclass of the Translator class that is specific to assigning values in the debugger.
    /// </summary>
    internal class AssignmentTranslator : Translator
    {
        private DebugCompilerContext _context;

        public AssignmentTranslator(DebugCompilerContext context)
            : base(context)
        {
            _context = context;
        }

        public override void TranslateInput()
        {
            _context.Emitter.BeginProgram(_context.ClassName, _context.Importer.ImportedAssemblies);
            MethodGenerator.BeginMethod(_context.MethodName, IrisType.Void, _context.ParameterVariables, _context.LocalVariables, false, string.Empty);

            // Parse the L-Value.
            // We use a seperate lexer for this because it's part of a different string.
            Symbol lvalue;
            byte[] buffer = Encoding.Default.GetBytes(_context.AssignmentLValue);
            MemoryStream input = new MemoryStream(buffer);
            using (StreamReader reader = new StreamReader(input))
            {
                Lexer lvalLexer = Lexer.Create(reader, _context.CompileErrors);
                lvalue = ParseLValue(lvalLexer);
                if (lvalue == null)
                    return; // Parsing the L-Value failed.  (Error message already generated)
            }

            IrisType lhs = lvalue.Type;

            // Parse the R-Value.
            IrisType rhs = ParseExpression();
            if (!Accept(Token.Eof))
                AddErrorAtTokenStart("Unexpected text after expression.");

            // Now finish emitting the code to do the assignment.
            if (rhs != IrisType.Invalid)
            {
                bool hasElementType = false;
                if (lhs.IsByRef || lhs.IsArray)
                {
                    hasElementType = true;
                    lhs = lhs.GetElementType();
                }

                if (lhs != rhs)
                {
                    AddErrorAtLastParsedPosition("Cannot assign value.  Expression type doesn't match value type");
                }
                else if (hasElementType)
                {
                    if (lvalue.Type.IsArray)
                        MethodGenerator.StoreElement(lhs);
                    else
                        MethodGenerator.Store(lhs);
                }
                else
                {
                    EmitStoreSymbol(lvalue);
                }
            }

            if (_context.ErrorCount == 0)
            {
                MethodGenerator.EndMethod();
                _context.Emitter.EndProgram();
            }
        }

        private Symbol ParseLValue(Lexer lvalLexer)
        {
            if (lvalLexer.CurrentToken != Token.Identifier)
            {
                AddLValueError();
                return null;
            }

            Symbol symbol = _context.SymbolTable.Lookup(lvalLexer.GetLexeme());
            if (symbol == null)
            {
                AddLValueError();
                return null;
            }

            IrisType lhs = symbol.Type;
            lvalLexer.MoveNext();
            if (lhs.IsArray)
            {
                // We should have an open bracket (We don't support changing the array value itself)
                if (lvalLexer.CurrentToken != Token.ChrOpenBracket)
                {
                    AddLValueError();
                    return null;
                }

                EmitLoadSymbol(symbol, SymbolLoadMode.Raw);

                lvalLexer.MoveNext();
                if (lvalLexer.CurrentToken != Token.Number)
                {
                    AddLValueError();
                    return null;
                }

                int index = lvalLexer.ParseInteger();
                MethodGenerator.PushIntConst(index);
                lvalLexer.MoveNext();
                if (lvalLexer.CurrentToken != Token.ChrCloseBracket)
                {
                    AddLValueError();
                    return null;
                }
            }
            else if (lhs.IsByRef)
            {
                EmitLoadSymbol(symbol, SymbolLoadMode.Raw);
            }

            return symbol;
        }

        private void AddLValueError()
        {
            // We shouldn't get into this state because we get the L-value strings from the full
            // name that we generate.  The only way to get here are values that don't come from the
            // Iris compiler or bugs.  We'll show a generic message if we happen to get into this
            // state.
            AddError(FilePosition.Begin, "Cannot assign to this value");
        }
    }
}
