//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.XPP


/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocOpenFile/*" />
FUNCTION XMLDocOpenFile( cFileName AS STRING) AS INT64
    LOCAL nErrHandle AS USUAL
    VAR nResult := XMLDocOpenFile(cFileName, OUT nErrHandle)
    XDocument.RegisterError(nErrHandle)
    RETURN nResult


/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocOpenFile_2/*" />
FUNCTION XMLDocOpenFile( cFileName AS STRING, nErrHandle OUT USUAL) AS INT64
    LOCAL oDoc AS XDocument
    LOCAL nResult AS INT64
    nResult := 0

    oDoc := XDocument{}
    oDoc:OpenFile(cFileName)
    nResult := oDoc:DocHandle
    IF nResult == 0
        nErrHandle := oDoc:LastError
    ELSE
        nErrHandle := NIL
    ENDIF
    RETURN nResult



/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocSetAction/*" />
FUNCTION XMLDocSetAction( nDocHandle AS INT64, cNode AS STRING, bCallback AS CODEBLOCK) AS LONG
    LOCAL oDoc      AS XDocument
    LOCAL nActions  AS LONG
    oDoc := XDocument.GetDocument(nDocHandle)
    IF oDoc != NULL_OBJECT
        nActions := oDoc:SetAction(cNode, bCallback)
    ELSE
        nActions := 0
    ENDIF
RETURN nActions

/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocResetAction/*" />
FUNCTION XMLDocResetAction( nDocHandle AS INT64 ) AS LOGIC
    LOCAL oDoc    AS XDocument
    LOCAL lOk     AS LOGIC
    oDoc := XDocument.GetDocument(nDocHandle)
    IF oDoc != NULL_OBJECT
        oDoc:ClearActions()
        lOk := TRUE
    ELSE
        lOk := FALSE
    ENDIF
RETURN lOk

/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocProcess/*" />
FUNCTION XMLDocProcess(nDocHandle AS INT64) AS LOGIC
    LOCAL oDoc    AS XDocument
    LOCAL lOk     AS LOGIC
    oDoc := XDocument.GetDocument(nDocHandle)
    IF oDoc != NULL_OBJECT
        lOk := oDoc:ProcessNodes()
    ELSE
        lOk := FALSE
    ENDIF
RETURN lOk

/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocClose/*" />
FUNCTION XMLDocClose(nDocHandle AS INT64) AS LOGIC
    LOCAL oDoc  AS XDocument
    LOCAL lOk := FALSE AS LOGIC
    oDoc := XDocument.GetDocument(nDocHandle)
    IF oDoc != NULL_OBJECT
        lOk := oDoc:Close()
    ENDIF
RETURN lOk

/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocCloseAll/*" />
FUNCTION XMLDocCloseAll() AS LOGIC
    RETURN XDocument.CloseAllDocuments()


/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocClearErrors/*" />
FUNCTION XMLDocClearErrors() AS LOGIC
    RETURN XDocument.ClearAllErrors()


/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocGetErrorList/*" />
FUNCTION XMLDocGetErrorList( nDocHandle := -1 AS INT64, nErrHandle := -1 AS INT64 ) AS ARRAY
IF nDocHandle != -1
    VAR errors := XDocument.GetErrorsForDocument(nDocHandle)
    VAR aResult := {}
    FOREACH VAR error IN errors
        AAdd(aResult, error:ToArray())
    NEXT
    RETURN aResult
ELSEIF nErrHandle != -1
    LOCAL oError AS XError
    oError := XDocument.FindError(nErrHandle)
    IF oError != NULL
        RETURN { oError:ToArray() }
    ENDIF
ELSE
    VAR errors := XDocument.GetErrors()
    VAR aResult := {}
    FOREACH VAR error IN errors
        AAdd(aResult, error:ToArray())
    NEXT
    RETURN aResult
ENDIF
RETURN NULL_ARRAY


/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocGetRootTag/*" />
FUNCTION XMLDocGetRootTag( nDocHandle AS INT64 ) AS INT64
    LOCAL oDoc  AS XDocument
    LOCAL nTag  := 0 AS INT64
    oDoc := XDocument.GetDocument(nDocHandle)
    IF oDoc != NULL_OBJECT
        nTag := oDoc:RootId
    ENDIF
RETURN nTag

/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocOpenString/*" />
FUNCTION XMLDocOpenString( cXMLString AS STRING ,  nErrHandle OUT USUAL ) AS INT64
    LOCAL oDoc AS XDocument
    LOCAL nResult AS INT64
    nResult := 0

    oDoc := XDocument{}
    oDoc:OpenText(cXMLString)
    nResult := oDoc:DocHandle
    IF nResult == 0
        nErrHandle := oDoc:LastError
    ELSE
        nErrHandle := NIL
    ENDIF
    RETURN nResult

/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLDocOpenString_2/*" />
FUNCTION XMLDocOpenString( cXMLString AS STRING  ) AS INT64
    LOCAL nErrHandle AS USUAL
    VAR nResult := XMLDocOpenString(cXMLString, OUT nErrHandle)
    XDocument.RegisterError(nErrHandle)
    RETURN nResult


/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLGetAttribute/*" />
FUNCTION XMLGetAttribute( nTagHandle AS INT64, cAttributeName AS STRING ) AS STRING
    LOCAL aTag := NIL AS USUAL
    LOCAL aAttributes AS ARRAY
    IF XMLGetTag(nTagHandle, OUT aTag)
        IF IsArray(aTag)
            IF ALen(aTag) >= XMLTAG_ATTRIB .AND. IsArray(aTag[XMLTAG_ATTRIB])
                aAttributes := aTag[XMLTAG_ATTRIB]
                FOREACH element AS ARRAY IN aAttributes
                    IF String.Compare(element[TAGATTR_NAME], cAttributeName, TRUE) == 0
                        RETURN element[TAGATTR_VALUE]
                    ENDIF
                NEXT
            ENDIF
        ENDIF
    ENDIF
RETURN NULL_STRING


/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLGetChild/*" />
FUNCTION XMLGetChild( nTagHandle AS INT64, cChildTagName AS STRING) AS INT64
    RETURN XDocument.FindFirstChildTag(nTagHandle, cChildTagName)

/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLGetChildren/*" />
FUNCTION XMLGetChildren( nTagHandle AS INT64, cChildTagName AS STRING) AS ARRAY
    RETURN XDocument.FindAllChildTags(nTagHandle, cChildTagName)

/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLGetParent/*" />
FUNCTION XMLGetParent( nTagHandle AS INT64) AS INT64
    RETURN XDocument.FindParent(nTagHandle)

/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLGetTag/*" />
FUNCTION XMLGetTag( nTagHandle AS INT64, aTagMember OUT USUAL ) AS LOGIC
    LOCAL aResult AS ARRAY
    aResult := XDocument.FindTag(nTagHandle)
    IF aResult != NULL_ARRAY
        aTagMember := aResult
    ELSE
        aTagMember := NULL_ARRAY
    ENDIF

RETURN aResult != NULL_ARRAY


/// <include file="XSharp.XPP.Docs.xml" path="doc/XMLSelectNodes/*" />
FUNCTION XMLSelectNodes(nDocHandle AS INT64, cSelect AS STRING, aTagHandles OUT USUAL) AS LOGIC
    LOCAL oDoc    AS XDocument
    LOCAL lOk     AS LOGIC
    oDoc := XDocument.GetDocument(nDocHandle)
    IF oDoc != NULL_OBJECT
        aTagHandles := oDoc:SelectNodes(cSelect)
        lOk := TRUE
    ELSE
        aTagHandles := null_array
        lOk := FALSE
    ENDIF
    RETURN lOk


FUNCTION XmlDocExplainErrorMessage(nId AS LONG) AS STRING
SWITCH nId

CASE  XMLDOC_ERROR_NO_ERROR         ; RETURN "no error"
CASE  XMLDOC_ERROR_OUT_OF_MEMORY    ; RETURN "not enough memory "
CASE  XMLDOC_ERROR_INVALID_DTD_DECL ; RETURN "invalid DTD declaration"
CASE  XMLDOC_ERROR_INVALID_XML      ; RETURN "content outside XML tags"
CASE  XMLDOC_ERROR_ROOTTAG_EMPTY    ; RETURN "document has no root tag"
CASE  XMLDOC_ERROR_ENDTAG_MISSING   ; RETURN "invalid endtag or no endtag found"
CASE  XMLDOC_ERROR_EXPECT_DELIMIT   ; RETURN "expecting string delimiter"
CASE  XMLDOC_ERROR_UNTERM_STRING    ; RETURN "unterminated string"
CASE  XMLDOC_ERROR_FILE_NOT_FOUND   ; RETURN "file cannot be found"
CASE  XMLDOC_ERROR_READING_FILE     ; RETURN "file cannot be read"
CASE  XMLDOC_ERROR_NO_FILENAME      ; RETURN "no filename has been provided"
CASE  XMLDOC_ERROR_DUPLICATE_ATTR   ; RETURN "duplicate attribute"
CASE  XMLDOC_ERROR_MALFORMED_ATTR   ; RETURN "malformed attribute"
CASE  XMLDOC_ERROR_INVALID_ATTRNAME ; RETURN "invalid name for attribute"
CASE  XMLDOC_ERROR_PROCESS          ; RETURN "error WHILE processing action codeblocks"
END SWITCH
RETURN "unknown error"
