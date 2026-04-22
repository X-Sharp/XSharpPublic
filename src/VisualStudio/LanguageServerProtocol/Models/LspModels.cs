using System;
using System.Collections.Generic;

namespace XSharp.LanguageServerProtocol.Models
{
    public enum LspSymbolKind
    {
        File = 1,
        Module = 2,
        Namespace = 3,
        Package = 4,
        Class = 5,
        Method = 6,
        Property = 7,
        Field = 8,
        Constructor = 9,
        Enum = 10,
        Interface = 11,
        Function = 12,
        Variable = 13,
        Constant = 14,
        String = 15,
        Number = 16,
        Boolean = 17,
        Array = 18,
        Object = 19,
        Key = 20,
        Null = 21,
        EnumMember = 22,
        Struct = 23,
        Event = 24,
        Operator = 25,
        TypeParameter = 26
    }

    public enum LspCompletionItemKind
    {
        Text = 1,
        Method = 2,
        Function = 3,
        Constructor = 4,
        Field = 5,
        Variable = 6,
        Class = 7,
        Interface = 8,
        Module = 9,
        Property = 10,
        Unit = 11,
        Value = 12,
        Enum = 13,
        Keyword = 14,
        Snippet = 15,
        Color = 16,
        File = 17,
        Reference = 18,
        Folder = 19,
        EnumMember = 20,
        Constant = 21,
        Struct = 22,
        Event = 23,
        Operator = 24,
        TypeParameter = 25
    }

    public sealed class LspPosition
    {
        public int Line { get; set; }
        public int Character { get; set; }
    }

    public sealed class LspRange
    {
        public LspPosition Start { get; set; } = new LspPosition();
        public LspPosition End { get; set; } = new LspPosition();
    }

    public sealed class LspLocation
    {
        public string Uri { get; set; } = string.Empty;
        public LspRange Range { get; set; } = new LspRange();
    }

    public sealed class LspTextDocumentIdentifier
    {
        public string Uri { get; set; } = string.Empty;
    }

    public sealed class LspTextDocumentPositionParams
    {
        public LspTextDocumentIdentifier TextDocument { get; set; } = new LspTextDocumentIdentifier();
        public LspPosition Position { get; set; } = new LspPosition();
    }

    public sealed class LspReferenceParams : LspTextDocumentPositionParams
    {
        public bool IncludeDeclaration { get; set; } = true;
    }

    public sealed class LspCompletionParams : LspTextDocumentPositionParams
    {
        public string? TriggerCharacter { get; set; }
    }

    public sealed class LspCompletionItem
    {
        public string Label { get; set; } = string.Empty;
        public string? Detail { get; set; }
        public string? Documentation { get; set; }
        public LspCompletionItemKind Kind { get; set; } = LspCompletionItemKind.Text;
    }

    public sealed class LspHover
    {
        public string Contents { get; set; } = string.Empty;
        public LspRange? Range { get; set; }
    }

    public sealed class LspParameterInformation
    {
        public string Label { get; set; } = string.Empty;
        public string? Documentation { get; set; }
    }

    public sealed class LspSignatureInformation
    {
        public string Label { get; set; } = string.Empty;
        public string? Documentation { get; set; }
        public IReadOnlyList<LspParameterInformation> Parameters { get; set; } = Array.Empty<LspParameterInformation>();
    }

    public sealed class LspSignatureHelp
    {
        public IReadOnlyList<LspSignatureInformation> Signatures { get; set; } = Array.Empty<LspSignatureInformation>();
        public int ActiveSignature { get; set; }
        public int ActiveParameter { get; set; }
    }

    public sealed class LspDocumentSymbol
    {
        public string Name { get; set; } = string.Empty;
        public string? Detail { get; set; }
        public LspSymbolKind Kind { get; set; }
        public LspRange Range { get; set; } = new LspRange();
        public LspRange SelectionRange { get; set; } = new LspRange();
        public IReadOnlyList<LspDocumentSymbol> Children { get; set; } = Array.Empty<LspDocumentSymbol>();
    }

    public sealed class LspWorkspaceSymbol
    {
        public string Name { get; set; } = string.Empty;
        public string? ContainerName { get; set; }
        public LspSymbolKind Kind { get; set; }
        public LspLocation Location { get; set; } = new LspLocation();
    }
}
