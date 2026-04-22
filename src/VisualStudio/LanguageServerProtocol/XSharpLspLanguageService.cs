using System;
using System.Collections.Generic;
using System.Linq;
using XSharp.LanguageServerProtocol.Internal;
using XSharp.LanguageServerProtocol.Models;
using XSharpModel;

namespace XSharp.LanguageServerProtocol
{
    public sealed class XSharpLspLanguageService : IDisposable
    {
        private readonly XSharpLspWorkspace _workspace;

        public XSharpLspLanguageService(XSharpLspProjectOptions options)
        {
            _workspace = new XSharpLspWorkspace(options ?? throw new ArgumentNullException(nameof(options)));
        }

        public void OpenDocument(string uri, string text)
        {
            _workspace.OpenOrUpdate(uri, text);
        }

        public void UpdateDocument(string uri, string text)
        {
            _workspace.OpenOrUpdate(uri, text);
        }

        public void CloseDocument(string uri)
        {
            _workspace.Close(uri);
        }

        public LspHover? Hover(LspTextDocumentPositionParams @params)
        {
            if (!TryResolveSymbol(@params, out var state, out var symbol))
            {
                return null;
            }

            var contents = symbol.Description;
            if (symbol is XSourceEntity source && !string.IsNullOrWhiteSpace(source.XmlComments))
            {
                contents += Environment.NewLine + Environment.NewLine + source.XmlComments;
            }

            return new LspHover
            {
                Contents = contents,
                Range = ToRange(state, symbol)
            };
        }

        public IReadOnlyList<LspCompletionItem> Completion(LspCompletionParams @params)
        {
            if (!_workspace.TryGet(@params.TextDocument.Uri, out var state))
            {
                return Array.Empty<LspCompletionItem>();
            }

            var offset = state.ToOffset(@params.Position);
            var prefix = GetIdentifierPrefix(state.Text, offset);
            var line = @params.Position.Line;
            var member = state.File.FindMemberAtPosition(offset) as XSourceMemberSymbol;

            var results = new Dictionary<string, LspCompletionItem>(StringComparer.OrdinalIgnoreCase);

            AddCompletions(results, state.File.EntityList.Select(e => (IXSymbol)e), prefix);
            AddCompletions(results, _workspace.Project.GetTypesLike(prefix, BuildUsings(state.File, line)).Cast<IXSymbol>(), prefix);
            AddCompletions(results, _workspace.Project.FindFunctionsLike(prefix, false).Cast<IXSymbol>(), prefix);
            AddCompletions(results, _workspace.Project.FindGlobalMembersLike(prefix, false).Cast<IXSymbol>(), prefix);
            AddNamespaceCompletions(results, _workspace.Project.AllNamespaces, prefix);

            if (member != null)
            {
                AddCompletions(results, member.Parameters.Cast<IXSymbol>(), prefix);
                foreach (var local in GetLocals(member, state))
                {
                    AddCompletions(results, new[] { (IXSymbol)local }, prefix);
                }

                if (member.ParentType != null)
                {
                    AddCompletions(results, member.ParentType.AllMembers.Cast<IXSymbol>(), prefix);
                }
            }

            return results.Values.OrderBy(v => v.Label, StringComparer.OrdinalIgnoreCase).ToArray();
        }

        public LspLocation? Definition(LspTextDocumentPositionParams @params)
        {
            if (!TryResolveSymbol(@params, out _, out var symbol))
            {
                return null;
            }

            return ToLocation(symbol);
        }

        public IReadOnlyList<LspLocation> References(LspReferenceParams @params)
        {
            if (!TryResolveSymbol(@params, out var state, out var symbol))
            {
                return Array.Empty<LspLocation>();
            }

            var name = symbol.Name;
            var locations = new List<LspLocation>();
            var declaration = ToLocation(symbol);
            if (@params.IncludeDeclaration && declaration != null)
            {
                locations.Add(declaration);
            }

            foreach (var document in _workspace.Documents)
            {
                foreach (var range in FindWordRanges(document.Text, name))
                {
                    var line = document.ToPosition(range.start);
                    if (!@params.IncludeDeclaration && declaration != null && SameLocation(document.Uri.AbsoluteUri, declaration, range.start, range.end, document))
                    {
                        continue;
                    }

                    locations.Add(new LspLocation
                    {
                        Uri = document.Uri.AbsoluteUri,
                        Range = new LspRange
                        {
                            Start = line,
                            End = document.ToPosition(range.end)
                        }
                    });
                }
            }

            return locations
                .GroupBy(l => $"{l.Uri}:{l.Range.Start.Line}:{l.Range.Start.Character}:{l.Range.End.Line}:{l.Range.End.Character}")
                .Select(g => g.First())
                .ToArray();
        }

        public IReadOnlyList<LspDocumentSymbol> DocumentSymbols(LspTextDocumentIdentifier document)
        {
            if (!_workspace.TryGet(document.Uri, out var state))
            {
                return Array.Empty<LspDocumentSymbol>();
            }

            var entities = state.File.EntityList.OfType<XSourceEntity>().ToArray();
            var byParent = entities.ToLookup(e => e.Parent as XSourceEntity);

            IReadOnlyList<LspDocumentSymbol> BuildChildren(XSourceEntity? parent)
            {
                return byParent[parent]
                    .Where(e => e.Kind != Kind.Local && e.Kind != Kind.Parameter)
                    .Select(e => new LspDocumentSymbol
                    {
                        Name = e.Name,
                        Detail = e.Prototype,
                        Kind = ToSymbolKind(e.Kind),
                        Range = ToRange(state, e),
                        SelectionRange = ToRange(state, e),
                        Children = BuildChildren(e)
                    })
                    .ToArray();
            }

            return BuildChildren(null);
        }

        public IReadOnlyList<LspWorkspaceSymbol> WorkspaceSymbols(string query)
        {
            query ??= string.Empty;
            var symbols = new List<LspWorkspaceSymbol>();
            foreach (var document in _workspace.Documents)
            {
                foreach (var symbol in document.File.EntityList.OfType<XSourceEntity>())
                {
                    if (!string.IsNullOrEmpty(query) &&
                        symbol.Name.IndexOf(query, StringComparison.OrdinalIgnoreCase) < 0)
                    {
                        continue;
                    }

                    var location = ToLocation(symbol);
                    if (location == null)
                    {
                        continue;
                    }

                    symbols.Add(new LspWorkspaceSymbol
                    {
                        Name = symbol.Name,
                        ContainerName = symbol.Parent?.FullName,
                        Kind = ToSymbolKind(symbol.Kind),
                        Location = location
                    });
                }
            }

            return symbols.ToArray();
        }

        public LspSignatureHelp? SignatureHelp(LspTextDocumentPositionParams @params)
        {
            if (!_workspace.TryGet(@params.TextDocument.Uri, out var state))
            {
                return null;
            }

            var offset = state.ToOffset(@params.Position);
            var invocation = FindInvocationAtPosition(state, offset);
            if (invocation.member == null)
            {
                return null;
            }

            var member = invocation.member;
            var signature = new LspSignatureInformation
            {
                Label = member.Prototype,
                Documentation = member.Description,
                Parameters = member.Parameters
                    .Select(p => new LspParameterInformation
                    {
                        Label = p.Name,
                        Documentation = p.Description
                    })
                    .ToArray()
            };

            return new LspSignatureHelp
            {
                Signatures = new[] { signature },
                ActiveSignature = 0,
                ActiveParameter = ClampActiveParameter(invocation.activeParameter, member.Parameters.Count)
            };
        }

        public void Dispose()
        {
            _workspace.Dispose();
        }

        private bool TryResolveSymbol(LspTextDocumentPositionParams @params, out DocumentState state, out IXSymbol symbol)
        {
            symbol = null!;
            if (!_workspace.TryGet(@params.TextDocument.Uri, out state!))
            {
                return false;
            }

            var offset = state.ToOffset(@params.Position);
            var line = @params.Position.Line;
            var identifier = GetIdentifierAt(state.Text, offset);
            if (string.IsNullOrWhiteSpace(identifier))
            {
                return false;
            }

            symbol = ResolveIdentifier(state, line, offset, identifier);
            return symbol != null;
        }

        private IXSymbol ResolveIdentifier(DocumentState state, int line, int offset, string identifier)
        {
            var file = state.File;
            var member = file.FindMemberAtPosition(offset) as XSourceMemberSymbol ?? file.FindMemberAtRow(line) as XSourceMemberSymbol;

            if (member != null)
            {
                if (string.Equals(member.Name, identifier, StringComparison.OrdinalIgnoreCase))
                {
                    return member;
                }

                var parameter = member.Parameters.FirstOrDefault(p => string.Equals(p.Name, identifier, StringComparison.OrdinalIgnoreCase));
                if (parameter != null)
                {
                    return parameter;
                }

                var local = GetLocals(member, state)
                    .Where(v => v.Range.StartLine <= line)
                    .LastOrDefault(v => string.Equals(v.Name, identifier, StringComparison.OrdinalIgnoreCase));
                if (local != null)
                {
                    return local;
                }

                if (member.ParentType != null)
                {
                    var typeMember = member.ParentType.GetMembers(identifier, false).FirstOrDefault();
                    if (typeMember != null)
                    {
                        return typeMember;
                    }
                }
            }

            var currentNamespace = file.EntityList
                .OfType<XSourceEntity>()
                .Where(e => e.Kind == Kind.Namespace && e.IncludesLine(line))
                .LastOrDefault()?.FullName ?? string.Empty;

            var usings = BuildUsings(file, line, currentNamespace);

            var type = _workspace.Project.FindType(identifier, usings);
            if (type != null)
            {
                return type;
            }

            var global = _workspace.Project.FindGlobalOrDefine(identifier);
            if (global != null)
            {
                return global;
            }

            var function = _workspace.Project.FindFunction(identifier);
            if (function != null)
            {
                return function;
            }

            return file.EntityList.LastOrDefault(e => string.Equals(e.Name, identifier, StringComparison.OrdinalIgnoreCase));
        }

        private static IList<string> BuildUsings(XFile file, int line, string currentNamespace = "")
        {
            var values = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var u in file.Usings)
            {
                values.Add(u);
            }

            foreach (var u in file.StaticUsings)
            {
                values.Add(u);
            }

            var ns = string.IsNullOrEmpty(currentNamespace)
                ? file.EntityList.OfType<XSourceEntity>()
                    .Where(e => e.Kind == Kind.Namespace && e.IncludesLine(line))
                    .LastOrDefault()?.FullName
                : currentNamespace;

            if (!string.IsNullOrWhiteSpace(ns))
            {
                var parts = ns!.Split('.');
                var full = string.Empty;
                foreach (var part in parts)
                {
                    full = string.IsNullOrEmpty(full) ? part : full + "." + part;
                    values.Add(full);
                }
            }

            return values.ToList();
        }

        private static IList<XSourceVariableSymbol> GetLocals(XSourceMemberSymbol member, DocumentState state)
        {
            var walker = new SourceWalker(new XFile(member.File.FullPath, member.File.Project), false);
            var start = member.Interval.Start;
            var width = Math.Max(0, member.Interval.Width);
            if (start + width > state.Text.Length)
            {
                width = state.Text.Length - start;
            }

            var source = (start >= 0 && width > 0 && start < state.Text.Length)
                ? state.Text.Substring(start, width)
                : state.Text;

            var locals = walker.ParseLocals(source, member);
            foreach (var local in locals)
            {
                local.Parent = member;
                local.File = member.File;
            }

            if (member.Kind.IsClassMember(member.File.Project.Dialect) && !member.Modifiers.HasFlag(Modifiers.Static))
            {
                locals.Add(new XSourceVariableSymbol(member, "SELF", member.Range, member.Interval, member.ParentName) { File = member.File });
                if (member.ParentType != null && !string.IsNullOrEmpty(member.ParentType.BaseTypeName))
                {
                    locals.Add(new XSourceVariableSymbol(member, "SUPER", member.Range, member.Interval, member.ParentType.BaseTypeName) { File = member.File });
                }
            }

            return locals;
        }

        private static void AddCompletions(Dictionary<string, LspCompletionItem> target, IEnumerable<IXSymbol> symbols, string prefix)
        {
            foreach (var symbol in symbols)
            {
                if (symbol == null || string.IsNullOrWhiteSpace(symbol.Name))
                {
                    continue;
                }

                if (!string.IsNullOrEmpty(prefix) && !symbol.Name.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                {
                    continue;
                }

                var key = symbol.Name + "|" + symbol.Kind;
                if (target.ContainsKey(key))
                {
                    continue;
                }

                target[key] = new LspCompletionItem
                {
                    Label = symbol.Name,
                    Detail = symbol.Prototype,
                    Documentation = symbol.Description,
                    Kind = ToCompletionKind(symbol.Kind)
                };
            }
        }

        private static void AddNamespaceCompletions(Dictionary<string, LspCompletionItem> target, IEnumerable<string> namespaces, string prefix)
        {
            foreach (var ns in namespaces)
            {
                if (string.IsNullOrWhiteSpace(ns))
                {
                    continue;
                }

                if (!string.IsNullOrEmpty(prefix) && !ns.StartsWith(prefix, StringComparison.OrdinalIgnoreCase))
                {
                    continue;
                }

                var key = ns + "|" + Kind.Namespace;
                if (target.ContainsKey(key))
                {
                    continue;
                }

                target[key] = new LspCompletionItem
                {
                    Label = ns,
                    Detail = ns,
                    Documentation = "Namespace",
                    Kind = LspCompletionItemKind.Module
                };
            }
        }

        private static int ClampActiveParameter(int activeParameter, int parameterCount)
        {
            if (parameterCount <= 0)
            {
                return 0;
            }

            if (activeParameter < 0)
            {
                return 0;
            }

            if (activeParameter >= parameterCount)
            {
                return parameterCount - 1;
            }

            return activeParameter;
        }

        private static LspLocation? ToLocation(IXSymbol symbol)
        {
            if (!(symbol is IXSourceSymbol source) || source.File == null)
            {
                return null;
            }

            var uri = new Uri(source.File.FullPath, UriKind.Absolute).AbsoluteUri;
            return new LspLocation
            {
                Uri = uri,
                Range = new LspRange
                {
                    Start = new LspPosition { Line = source.Range.StartLine, Character = source.Range.StartColumn },
                    End = new LspPosition { Line = source.Range.EndLine, Character = source.Range.EndColumn + 1 }
                }
            };
        }

        private static LspRange ToRange(DocumentState state, IXSymbol symbol)
        {
            if (symbol is IXSourceSymbol source)
            {
                return new LspRange
                {
                    Start = new LspPosition { Line = source.Range.StartLine, Character = source.Range.StartColumn },
                    End = new LspPosition { Line = source.Range.EndLine, Character = source.Range.EndColumn + 1 }
                };
            }

            return new LspRange
            {
                Start = new LspPosition(),
                End = new LspPosition()
            };
        }

        private static string GetIdentifierAt(string text, int offset)
        {
            if (string.IsNullOrEmpty(text))
            {
                return string.Empty;
            }

            var index = Math.Max(0, Math.Min(offset, text.Length - 1));
            if (!IsIdentifierChar(text[index]) && index > 0)
            {
                index--;
            }

            if (!IsIdentifierChar(text[index]))
            {
                return string.Empty;
            }

            var start = index;
            var end = index;
            while (start > 0 && IsIdentifierChar(text[start - 1])) start--;
            while (end + 1 < text.Length && IsIdentifierChar(text[end + 1])) end++;
            return text.Substring(start, end - start + 1);
        }

        private static string GetIdentifierPrefix(string text, int offset)
        {
            if (string.IsNullOrEmpty(text))
            {
                return string.Empty;
            }

            var i = Math.Max(0, Math.Min(offset - 1, text.Length - 1));
            if (i < 0 || !IsIdentifierChar(text[i]))
            {
                return string.Empty;
            }

            var start = i;
            while (start > 0 && IsIdentifierChar(text[start - 1])) start--;
            return text.Substring(start, i - start + 1);
        }

        private static bool IsIdentifierChar(char c)
        {
            return char.IsLetterOrDigit(c) || c == '_' || c == '@';
        }

        private static IEnumerable<(int start, int end)> FindWordRanges(string text, string word)
        {
            if (string.IsNullOrEmpty(text) || string.IsNullOrEmpty(word))
            {
                yield break;
            }

            var index = 0;
            while (true)
            {
                index = text.IndexOf(word, index, StringComparison.OrdinalIgnoreCase);
                if (index < 0)
                {
                    yield break;
                }

                var leftOk = index == 0 || !IsIdentifierChar(text[index - 1]);
                var rightIndex = index + word.Length;
                var rightOk = rightIndex >= text.Length || !IsIdentifierChar(text[rightIndex]);
                if (leftOk && rightOk)
                {
                    yield return (index, rightIndex);
                }

                index += word.Length;
            }
        }

        private static bool SameLocation(string uri, LspLocation declaration, int start, int end, DocumentState document)
        {
            if (!string.Equals(uri, declaration.Uri, StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }

            var startPos = document.ToPosition(start);
            var endPos = document.ToPosition(end);
            return startPos.Line == declaration.Range.Start.Line &&
                   startPos.Character == declaration.Range.Start.Character &&
                   endPos.Line == declaration.Range.End.Line &&
                   endPos.Character == declaration.Range.End.Character;
        }

        private (IXMemberSymbol? member, int activeParameter) FindInvocationAtPosition(DocumentState state, int offset)
        {
            var text = state.Text;
            if (string.IsNullOrEmpty(text))
            {
                return (null, 0);
            }

            var i = Math.Max(0, Math.Min(offset, text.Length));
            var depth = 0;
            var commas = 0;

            for (var p = i - 1; p >= 0; p--)
            {
                var ch = text[p];
                if (ch == ')')
                {
                    depth++;
                    continue;
                }

                if (ch == '(')
                {
                    if (depth == 0)
                    {
                        var name = ReadIdentifierLeft(text, p - 1);
                        if (string.IsNullOrEmpty(name))
                        {
                            return (null, 0);
                        }

                        var line = state.ToPosition(p).Line;
                        var symbol = ResolveIdentifier(state, line, p, name);
                        if (symbol is IXMemberSymbol member)
                        {
                            return (member, commas);
                        }

                        return (null, 0);
                    }

                    depth--;
                    continue;
                }

                if (ch == ',' && depth == 0)
                {
                    commas++;
                }
            }

            return (null, 0);
        }

        private static string ReadIdentifierLeft(string text, int index)
        {
            var i = index;
            while (i >= 0 && char.IsWhiteSpace(text[i])) i--;
            var end = i;
            while (i >= 0 && (IsIdentifierChar(text[i]) || text[i] == '.')) i--;
            if (end < 0 || i == end)
            {
                return string.Empty;
            }

            var value = text.Substring(i + 1, end - i);
            var dot = value.LastIndexOf('.');
            return dot >= 0 ? value.Substring(dot + 1) : value;
        }

        private static LspCompletionItemKind ToCompletionKind(Kind kind)
        {
            switch (kind)
            {
                case Kind.Class:
                    return LspCompletionItemKind.Class;
                case Kind.Structure:
                case Kind.VOStruct:
                case Kind.Union:
                    return LspCompletionItemKind.Struct;
                case Kind.Interface:
                    return LspCompletionItemKind.Interface;
                case Kind.Namespace:
                    return LspCompletionItemKind.Module;
                case Kind.Constructor:
                    return LspCompletionItemKind.Constructor;
                case Kind.Method:
                case Kind.Access:
                case Kind.Assign:
                case Kind.LocalFunc:
                case Kind.LocalProc:
                    return LspCompletionItemKind.Method;
                case Kind.Function:
                case Kind.Procedure:
                    return LspCompletionItemKind.Function;
                case Kind.Property:
                    return LspCompletionItemKind.Property;
                case Kind.Field:
                case Kind.Local:
                case Kind.Parameter:
                case Kind.MemVar:
                case Kind.DbField:
                    return LspCompletionItemKind.Variable;
                case Kind.Event:
                    return LspCompletionItemKind.Event;
                case Kind.Enum:
                    return LspCompletionItemKind.Enum;
                case Kind.EnumMember:
                    return LspCompletionItemKind.EnumMember;
                case Kind.VODefine:
                case Kind.Define:
                case Kind.VOGlobal:
                    return LspCompletionItemKind.Constant;
                case Kind.Operator:
                    return LspCompletionItemKind.Operator;
                case Kind.TypeParameter:
                    return LspCompletionItemKind.TypeParameter;
                default:
                    return LspCompletionItemKind.Text;
            }
        }

        private static LspSymbolKind ToSymbolKind(Kind kind)
        {
            switch (kind)
            {
                case Kind.Class:
                    return LspSymbolKind.Class;
                case Kind.Structure:
                case Kind.VOStruct:
                case Kind.Union:
                    return LspSymbolKind.Struct;
                case Kind.Interface:
                    return LspSymbolKind.Interface;
                case Kind.Namespace:
                    return LspSymbolKind.Namespace;
                case Kind.Constructor:
                    return LspSymbolKind.Constructor;
                case Kind.Method:
                case Kind.Access:
                case Kind.Assign:
                case Kind.LocalFunc:
                case Kind.LocalProc:
                    return LspSymbolKind.Method;
                case Kind.Function:
                case Kind.Procedure:
                    return LspSymbolKind.Function;
                case Kind.Property:
                    return LspSymbolKind.Property;
                case Kind.Field:
                    return LspSymbolKind.Field;
                case Kind.Local:
                case Kind.Parameter:
                case Kind.MemVar:
                case Kind.DbField:
                    return LspSymbolKind.Variable;
                case Kind.Event:
                    return LspSymbolKind.Event;
                case Kind.Enum:
                    return LspSymbolKind.Enum;
                case Kind.EnumMember:
                    return LspSymbolKind.EnumMember;
                case Kind.Operator:
                    return LspSymbolKind.Operator;
                case Kind.TypeParameter:
                    return LspSymbolKind.TypeParameter;
                default:
                    return LspSymbolKind.Object;
            }
        }
    }
}
