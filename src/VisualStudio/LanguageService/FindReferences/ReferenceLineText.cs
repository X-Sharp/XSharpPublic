//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using LanguageService.SyntaxTree;
using Microsoft.VisualStudio.Language.StandardClassification;
using Microsoft.VisualStudio.Text.Classification;

namespace XSharp.LanguageService.FindReferences
{
    /// <summary>
    /// The Text column of a hit: the trimmed source line in the editor colors with the hit highlighted, like the C# entries.
    /// </summary>
    internal static class ReferenceLineText
    {
        internal readonly struct Segment
        {
            internal Segment(string text, string classification, bool isHit)
            {
                Text = text;
                Classification = classification;
                IsHit = isHit;
            }
            internal string Text { get; }
            internal string Classification { get; }
            internal bool IsHit { get; }
        }

        /// <summary>
        /// Classifies the tokens of one line by the lexer rules of the XSharpClassifier
        /// (without its extra keywords and its bracketed string rule).
        /// </summary>
        internal static List<ClassifiedRange> Classify(IList<IToken> tokens, int lineStart, int lineEnd)
        {
            var result = new List<ClassifiedRange>();
            // the tokens are ordered by position: find the first one that ends in the line
            int lo = 0, hi = tokens.Count;
            while (lo < hi)
            {
                int mid = (lo + hi) / 2;
                if (tokens[mid].StopIndex < lineStart)
                    lo = mid + 1;
                else
                    hi = mid;
            }
            for (int i = lo; i < tokens.Count && tokens[i].StartIndex < lineEnd; i++)
            {
                var token = tokens[i];
                var classification = ClassificationOf(token);
                if (classification == null || token.StartIndex < 0)
                    continue;
                var start = Math.Max(token.StartIndex, lineStart);
                var end = Math.Min(token.StopIndex + 1, lineEnd);
                if (end > start)
                    result.Add(new ClassifiedRange(start - lineStart, end - start, classification));
            }
            return result;
        }

        private static string ClassificationOf(IToken token)
        {
            var type = token.Type;
            switch (token.Channel)
            {
                case XSharpLexer.PREPROCESSORCHANNEL:
                    return PredefinedClassificationTypeNames.PreprocessorKeyword;
                case XSharpLexer.DEFOUTCHANNEL:
                    return PredefinedClassificationTypeNames.ExcludedCode;
                case XSharpLexer.XMLDOCCHANNEL:
                case XSharpLexer.Hidden:
                    return XSharpLexer.IsComment(type) ? PredefinedClassificationTypeNames.Comment : null;
            }
            if (type == XSharpLexer.Eof || XSharpLexer.IsIdentifier(type))
                return null;
            if (XSharpLexer.IsConstant(type))
            {
                switch (type)
                {
                    case XSharpLexer.STRING_CONST:
                    case XSharpLexer.CHAR_CONST:
                    case XSharpLexer.ESCAPED_STRING_CONST:
                    case XSharpLexer.INTERPOLATED_STRING_CONST:
                    case XSharpLexer.INCOMPLETE_STRING_CONST:
                    case XSharpLexer.BRACKETED_STRING_CONST:
                    case XSharpLexer.TEXT_STRING_CONST:
                        return PredefinedClassificationTypeNames.String;
                    case XSharpLexer.FALSE_CONST:
                    case XSharpLexer.TRUE_CONST:
                        return PredefinedClassificationTypeNames.Keyword;
                    case XSharpLexer.VO_AND:
                    case XSharpLexer.VO_NOT:
                    case XSharpLexer.VO_OR:
                    case XSharpLexer.VO_XOR:
                    case XSharpLexer.SYMBOL_CONST:
                    case XSharpLexer.NIL:
                        return PredefinedClassificationTypeNames.Literal;
                    default:
                        if (type >= XSharpLexer.FIRST_NULL && type <= XSharpLexer.LAST_NULL)
                            return PredefinedClassificationTypeNames.Keyword;
                        return PredefinedClassificationTypeNames.Number;
                }
            }
            if (XSharpLexer.IsKeyword(type) || XSharpLexer.IsWordOperator(type))
                return PredefinedClassificationTypeNames.Keyword;
            if (XSharpLexer.IsOperator(type))
                return PredefinedClassificationTypeNames.Operator;
            return null;
        }

        /// <summary>
        /// Splits the trimmed line into runs of equal classification; the hit is a run of its own.
        /// A hit outside the line is ignored.
        /// </summary>
        internal static List<Segment> Segments(string lineText, int column, int length, IReadOnlyList<ClassifiedRange> classifications)
        {
            lineText = lineText ?? "";
            var first = lineText.Length - lineText.TrimStart().Length;
            var last = lineText.TrimEnd().Length;    // exclusive
            var types = new string[lineText.Length];
            foreach (var range in classifications ?? new ClassifiedRange[0])
            {
                for (int i = Math.Max(range.Start, 0); i < range.Start + range.Length && i < types.Length; i++)
                    types[i] = range.Classification;
            }
            bool hitInLine = length > 0 && column >= first && column + length <= last;
            var result = new List<Segment>();
            int segmentStart = first;
            for (int i = first + 1; i <= last; i++)
            {
                if (i == last || types[i] != types[segmentStart] || IsHit(i) != IsHit(segmentStart))
                {
                    result.Add(new Segment(lineText.Substring(segmentStart, i - segmentStart), types[segmentStart], IsHit(segmentStart)));
                    segmentStart = i;
                }
            }
            return result;

            bool IsHit(int position) => hitInLine && position >= column && position < column + length;
        }

        internal static FrameworkElement CreateTextBlock(ReferenceHit hit, ReferenceTextFormat format)
        {
            var block = new TextBlock { TextWrapping = TextWrapping.NoWrap };
            foreach (var segment in Segments(hit.LineText, hit.Column, hit.Length, hit.Classifications))
            {
                var run = new Run(segment.Text);
                var foreground = format?.Foreground(segment.Classification);
                if (foreground != null)
                    run.Foreground = foreground;
                if (segment.IsHit)
                {
                    if (format?.HitBackground != null)
                        run.Background = format.HitBackground;
                    else
                        run.FontWeight = FontWeights.Bold;
                }
                block.Inlines.Add(run);
            }
            return block;
        }
    }

    /// <summary>The editor colors for the Text column, read on the UI thread when the window shows the entries.</summary>
    internal sealed class ReferenceTextFormat
    {
        private readonly IClassificationFormatMap _formatMap;
        private readonly IClassificationTypeRegistryService _registry;

        internal ReferenceTextFormat(IClassificationFormatMap formatMap, IClassificationTypeRegistryService registry, Brush hitBackground)
        {
            _formatMap = formatMap;
            _registry = registry;
            HitBackground = hitBackground;
        }

        internal Brush HitBackground { get; }

        internal Brush Foreground(string classification)
        {
            if (classification == null || _formatMap == null || _registry == null)
                return null;
            var type = _registry.GetClassificationType(classification);
            return type == null ? null : _formatMap.GetTextProperties(type).ForegroundBrush;
        }
    }
}
