//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System.Collections.Generic

BEGIN NAMESPACE XSharpModel
/// <summary>
/// The types XProject.GetType builds from the database, kept while a search (Find All References) holds the
/// scope open. Thread static: only the thread that opened the scope sees the cache, and it is dropped when the outermost
/// scope is disposed, so a cached type never outlives the search that built it.
/// </summary>
CLASS XTypeCacheScope IMPLEMENTS IDisposable
    [ThreadStatic];
    STATIC PRIVATE depth AS INT
    [ThreadStatic];
    STATIC PRIVATE cache AS Dictionary<STRING, XSourceTypeSymbol>
    [ThreadStatic];
    STATIC PRIVATE extensions AS Dictionary<STRING, IList<IXMemberSymbol>>
    PRIVATE disposed AS LOGIC

    PRIVATE CONSTRUCTOR()
        depth += 1
        IF cache == NULL
            cache := Dictionary<STRING, XSourceTypeSymbol>{StringComparer.Ordinal}
            extensions := Dictionary<STRING, IList<IXMemberSymbol>>{StringComparer.Ordinal}
        ENDIF

    /// <summary>Opens the scope on this thread, or joins the one that is open already.</summary>
    STATIC METHOD Begin() AS XTypeCacheScope
        RETURN XTypeCacheScope{}

    /// <summary>A scope is open on this thread.</summary>
    STATIC PROPERTY IsOpen AS LOGIC GET depth > 0

    /// <summary>The type built for <paramref name="key"/> in this scope (also NULL); FALSE when nothing is cached or no scope is open.</summary>
    STATIC METHOD TryGet(key AS STRING, result OUT XSourceTypeSymbol) AS LOGIC
        result := NULL
        IF depth == 0 .OR. cache == NULL
            RETURN FALSE
        ENDIF
        RETURN cache:TryGetValue(key, OUT result)

    /// <summary>Keeps <paramref name="type"/> (NULL too) for <paramref name="key"/>; nothing without an open scope.</summary>
    STATIC METHOD Add(key AS STRING, type AS XSourceTypeSymbol) AS VOID
        IF depth > 0 .AND. cache != NULL
            cache[key] := type
        ENDIF
        RETURN

    /// <summary>The extension methods XProject.GetExtensions found for <paramref name="key"/> in this scope, as a new list
    /// (the caller may change it); FALSE when nothing is cached or no scope is open.</summary>
    STATIC METHOD TryGetExtensions(key AS STRING, result OUT IList<IXMemberSymbol>) AS LOGIC
        result := NULL
        IF depth == 0 .OR. extensions == NULL
            RETURN FALSE
        ENDIF
        IF extensions:TryGetValue(key, OUT VAR found)
            result := List<IXMemberSymbol>{found}
            RETURN TRUE
        ENDIF
        RETURN FALSE

    /// <summary>Keeps a copy of <paramref name="list"/> for <paramref name="key"/>; nothing without an open scope.</summary>
    STATIC METHOD AddExtensions(key AS STRING, list AS IList<IXMemberSymbol>) AS VOID
        IF depth > 0 .AND. extensions != NULL
            extensions[key] := List<IXMemberSymbol>{list}
        ENDIF
        RETURN

    /// <summary>The key of a GetType request: the looking project and every database row in its order.</summary>
    STATIC METHOD KeyOf(idProject AS INT64, found AS IList<XDbResult>) AS STRING
        VAR sb := System.Text.StringBuilder{}
        sb:Append(idProject):Append('|')
        FOREACH VAR element IN found
            sb:Append(element:IdType):Append(':'):Append(element:IdProject):Append(',')
        NEXT
        RETURN sb:ToString()

    /// <summary>Closes one level; the cache goes with the outermost one.</summary>
    METHOD Dispose() AS VOID
        IF disposed
            RETURN
        ENDIF
        disposed := TRUE
        depth -= 1
        IF depth <= 0
            depth := 0
            cache := NULL
            extensions := NULL
        ENDIF
        RETURN
END CLASS
END NAMESPACE
