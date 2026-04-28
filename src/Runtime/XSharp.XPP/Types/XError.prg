//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//



begin namespace XSharp.XPP
/// <include file="XSharp.XPP.Docs.xml" path="doc/XError/*" />
class XError
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XError.FileName/*" />
    property FileName as string auto
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XError.Line/*" />
    property Line     as long auto
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XError.Column/*" />
    property Column   as long auto
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XError.Id/*" />
    property Id       as int64 auto
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XError.Additional/*" />
    property Additional as usual auto
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XError.DocHandle/*" />
    property DocHandle as int64 auto
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XError.Handle/*" />
    property Handle    as int64 auto
    constructor(cFile as string, nId as int64)
        self:FileName := cFile
        self:Id       := nId
        self:Additional := ""
        self:Handle := XDocument.NewHandle()
    /// <include file="XSharp.XPP.Docs.xml" path="doc/XError.ToArray/*" />
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
