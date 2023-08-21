//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



begin namespace XSharp.XPP
/// <summary>Helper class that stores errors for an XML document</summary>
class XError
    /// <summary>Filename where the error occurred.</summary>
    property FileName as string auto
    /// <summary>Linenumber where the error occurred.</summary>
    property Line     as long auto
    /// <summary>Column where the error occurred.</summary>
    property Column   as long auto
    /// <summary>Id for the node where the error occurred.</summary>
    property Id       as int64 auto
    /// <summary>Additional info about the error.</summary>
    property Additional as usual auto
    /// <summary>Document Handle for which the error occurred.</summary>
    property DocHandle as int64 auto
    /// <summary>Error Handle.</summary>
    property Handle    as int64 auto
    constructor(cFile as string, nId as int64)
        self:FileName := cFile
        self:Id       := nId
        self:Additional := ""
        self:Handle := XDocument.NewHandle()
    /// <summary>Convert error object to XBase++ compatible error array.</summary>
    method ToArray() as array
        var aResult := ArrayNew(XML_ERROR_ADDINFO)
        aResult[XML_ERROR_ID]       := self:Id
        aResult[XML_ERROR_FILE]     := self:FileName
        aResult[XML_ERROR_LINE]     := self:Line
        aResult[XML_ERROR_COLUMN]   := self:Column
        aResult[XML_ERROR_ADDINFO]  := self:Additional
        return aResult

end class

end namespace
