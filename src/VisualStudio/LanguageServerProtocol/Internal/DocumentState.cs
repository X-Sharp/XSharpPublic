using System;
using System.Collections.Generic;
using XSharpModel;
using XSharp.LanguageServerProtocol.Models;

namespace XSharp.LanguageServerProtocol.Internal
{
    internal sealed class DocumentState
    {
        public DocumentState(Uri uri, string filePath, XFile file, string text)
        {
            Uri = uri;
            FilePath = filePath;
            File = file;
            UpdateText(text);
        }

        public Uri Uri { get; }

        public string FilePath { get; }

        public XFile File { get; }

        public string Text { get; private set; }

        public int[] LineStarts { get; private set; } = Array.Empty<int>();

        public void UpdateText(string text)
        {
            Text = text ?? string.Empty;
            LineStarts = BuildLineStarts(Text);
        }

        public int ToOffset(LspPosition position)
        {
            if (LineStarts.Length == 0)
            {
                return 0;
            }

            var line = Math.Max(0, Math.Min(position.Line, LineStarts.Length - 1));
            var lineStart = LineStarts[line];
            var lineEnd = line + 1 < LineStarts.Length ? LineStarts[line + 1] : Text.Length;
            var col = Math.Max(0, Math.Min(position.Character, Math.Max(0, lineEnd - lineStart)));
            return lineStart + col;
        }

        public LspPosition ToPosition(int offset)
        {
            var value = Math.Max(0, Math.Min(offset, Text.Length));
            var line = FindLine(value);
            return new LspPosition { Line = line, Character = value - LineStarts[line] };
        }

        private int FindLine(int offset)
        {
            var index = Array.BinarySearch(LineStarts, offset);
            if (index >= 0)
            {
                return index;
            }

            index = ~index;
            if (index <= 0)
            {
                return 0;
            }

            return index - 1;
        }

        private static int[] BuildLineStarts(string text)
        {
            var starts = new List<int> { 0 };
            for (var i = 0; i < text.Length; i++)
            {
                if (text[i] == '\n')
                {
                    starts.Add(i + 1);
                }
            }
            return starts.ToArray();
        }
    }
}
