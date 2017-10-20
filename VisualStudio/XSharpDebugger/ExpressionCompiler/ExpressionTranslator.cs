// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using Microsoft.VisualStudio.Debugger.Evaluation.ClrCompilation;

namespace XSharpDebugger.ExpressionCompiler
{
    /// <summary>
    /// A subclass of the Translator class that is specific to compiling expressions in the debugger.
    /// </summary>
    internal class ExpressionTranslator : Translator
    {
        private DebugCompilerContext _context;
        private Lexer _lexer;

        public ExpressionTranslator(DebugCompilerContext context)
            : base(context)
        {
            _context = context;
            _lexer = context.Lexer;
        }

        public override void TranslateInput()
        {
            // Parse the expression first to determine the result type
            MethodGenerator.SetOutputEnabled(false);
            IrisType resultType = ParseExpressionAndReadFormatSpecifiers();
            MethodGenerator.SetOutputEnabled(true);

            if (_context.ErrorCount == 0)
            {
                // No errors: Now that we know the result type, parse again and generate code this time

                _context.Emitter.BeginProgram(_context.ClassName, _context.Importer.ImportedAssemblies);
                _lexer.Reset();

                MethodGenerator.BeginMethod(_context.MethodName, resultType, _context.ParameterVariables, _context.LocalVariables, false, string.Empty);
                ParseExpression();
                MethodGenerator.EndMethod();

                bool readOnly = resultType.IsArray || resultType == IrisType.Void;
                if (_context.ErrorCount == 0)
                {
                    _context.Emitter.EndProgram();

                    if (!readOnly)
                    {
                        // As a final step, see if this expression is something that can be assigned
                        // to.  We only support very simple L-Values for assignments so if the syntax
                        // doesn't match exactly, make the result read only.
                        _lexer.Reset();
                        readOnly = !TryParseAsLValue();
                    }
                }

                if (resultType == IrisType.Boolean)
                {
                    // Setting the "BoolResult" flag allows the expression to be used for a
                    // "when true" breakpoint condition.
                    _context.ResultFlags |= DkmClrCompilationResultFlags.BoolResult;
                }

                if (ParsedCallSyntax)
                {
                    // If we parsed call syntax, this expression has the potential to have side
                    // effects.  Setting the PotentialSideEffect flag prevents the debugger from
                    // implicitly evaluating the expression without user interaction.
                    // Instead of implicity evaluating the expression, the debugger will show the
                    // message "This expression has side effects and will not be evaluated"
                    _context.ResultFlags |= DkmClrCompilationResultFlags.PotentialSideEffect;

                    readOnly = true; // Can't modify return value of call
                }

                if (readOnly)
                    _context.ResultFlags |= DkmClrCompilationResultFlags.ReadOnlyResult;
            }
        }

        private IrisType ParseExpressionAndReadFormatSpecifiers()
        {
            // Call the base class to parse the expression.
            // Then do our own parsing to handle format specifiers.
            IrisType resultType = ParseExpression();

            // If no compile errors, look for format specifiers
            if (_context.CompileErrors.Count > 0)
                return resultType;

            if (_lexer.CurrentToken == Token.Eof)
                return resultType;

            while (_lexer.CurrentToken == Token.ChrComma)
            {
                _lexer.MoveNext();
                if (_lexer.CurrentToken == Token.Identifier)
                {
                    _context.FormatSpecifiers.Add(_lexer.GetLexeme());
                    _lexer.MoveNext();
                }
                else
                {
                    AddErrorAtTokenStart("Invalid format specifier.");
                }
            }

            if (_lexer.CurrentToken != Token.Eof)
                AddErrorAtTokenStart("Unexpected text after expression.");

            return resultType;
        }

        private bool TryParseAsLValue()
        {
            if (!Accept(Token.Identifier))
                return false;

            if (Accept(Token.ChrOpenBracket))
            {
                // We allow assignment to array elements as long as the subscript is just a number.
                if (!Accept(Token.Number))
                    return false;
                if (!Accept(Token.ChrCloseBracket))
                    return false;
            }

            if (!Accept(Token.Eof))
                return false;

            return true;
        }
    }
}
