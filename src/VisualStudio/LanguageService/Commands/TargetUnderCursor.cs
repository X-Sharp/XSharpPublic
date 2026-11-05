//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System.Linq;
using Microsoft.VisualStudio.Text.Editor;
using XSharp.LanguageService.FindReferences;
using XSharpModel;

namespace XSharp.LanguageService.Commands
{
    /// <summary>
    /// The symbol a navigation command works on: the declaration under the cursor, or at a call site the overload
    /// that takes these arguments. Shared by Find References, Go To Implementation and Go To Base.
    /// Call it on the UI thread.
    /// </summary>
    internal static class TargetUnderCursor
    {
        /// <summary>The target, or null; <paramref name="candidates"/> is the number of overloads that fit the arguments.</summary>
        internal static IXSymbol Resolve(ITextView textView, out XSharpSearchLocation location, out CompletionState state, out int candidates)
        {
            candidates = 0;
            var symbols = textView.GetSymbolUnderCursor(out state, out location, out _);
            // on a declaration: the declared overload; on a call site: the overload that takes these arguments
            var name = location == null ? null : OverloadFilter.NameAt(location.Document?.Tokens, location.Position);
            IXSymbol target = name == null ? null : FindReferencesEngine.DeclaredEntity(location.File?.EntityList, name);
            if (target == null && location != null)
            {
                var searchLocation = location;
                var matches = OverloadFilter.Filter(SymbolResolver.ForCreation(symbols, location.Document?.Tokens, name),
                    OverloadFilter.ArgumentsAt(location.Document?.Tokens, location.Position),
                    id => SymbolResolver.TypeOf(searchLocation, id));
                target = matches.FirstOrDefault();
                candidates = matches.Count;
            }
            // Foo{} of a class without constructors: the lookup makes one up, take the class instead
            if (target != null && target.Kind == Kind.Constructor && target is IXMemberSymbol ctor &&
                ctor.ParentType != null && ctor.ParentType.GetConstructors().Length == 0)
            {
                target = ctor.ParentType;
            }
            return target;
        }
    }
}
