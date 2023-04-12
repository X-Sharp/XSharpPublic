//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//


USING System
USING System.Collections.Generic
USING System.Collections.Concurrent
USING System.Text
USING System.Xml
USING System.Linq

BEGIN NAMESPACE XSharp.XPP

    /// <summary>Helper class that is used by the XML functions</summary>

	CLASS XDocument
#region Static
        PROTECTED STATIC aDocuments AS List<XDocument>
        PROTECTED STATIC aErrors    AS List<XError>
        PROTECTED STATIC nextHandle AS INT64
        PROTECTED STATIC gate       AS OBJECT
        STATIC CONSTRUCTOR()
            Initialize()
            gate       := OBJECT{}

           /// <summary>Initialize the static variables</summary>
         STATIC METHOD Initialize() AS VOID
            aDocuments := List<XDocument>{}
            aErrors    := List<XError>{}
            nextHandle := 1
            return

        /// <summary>Find an open XDocument object by its unique handle</summary>
        STATIC METHOD GetDocument (nHandle AS INT64) AS XDocument
            FOREACH VAR doc IN aDocuments
                IF doc:DocHandle == nHandle
                    RETURN doc
                ENDIF
            NEXT
            RETURN NULL

        /// <summary>Allocate new XDocument handle</summary>
        STATIC METHOD NewHandle() AS INT64
            BEGIN LOCK gate
                RETURN nextHandle++
            END LOCK

        /// <summary>Find error by ID</summary>
        STATIC METHOD FindError(nError AS INT64) AS XError
            RETURN aErrors:Where( {x => x:Id == nError}):FirstOrDefault()

         /// <summary>Get list of all errors</summary>
        STATIC METHOD GetErrors() AS IEnumerable<XError>
            RETURN aErrors

        /// <summary>Get error for a specific document</summary>
        STATIC METHOD GetErrorsForDocument(nDocHandle AS INT64) AS IEnumerable<XError>
            RETURN aErrors:Where ( { x=> x:DocHandle == nDocHandle })

       /// <summary>Get ID for the parent of an XML node</summary>
       STATIC METHOD FindParent(nId AS INT64) AS INT64
            LOCAL oNode AS XmlNode
            LOCAL oDoc  AS XDocument
            oNode := FindNode(nId, OUT oDoc)
            IF oNode != NULL
                oNode := oNode:ParentNode
                IF oDoc:Nodes:ContainsKey(oNode)
                    RETURN oDoc:Nodes[oNode]
                ENDIF
            ENDIF
            RETURN 0

       /// <summary>Get XML node and document by ID</summary>
        STATIC PRIVATE METHOD FindNode(nID AS INT64, oDocResult OUT XDocument) AS XmlNode
            LOCAL oFoundNode := NULL AS XmlNode
            oDocResult := NULL
            BEGIN LOCK gate
                FOREACH oDoc AS XDocument IN aDocuments
                    IF oDoc:NodeIds:ContainsKey(nID)
                        oFoundNode := oDoc:NodeIds[nID]
                        oDocResult := oDoc
                        RETURN oFoundNode
                    ENDIF
                NEXT
            END LOCK
            RETURN NULL


        /// <summary>Get a list of the tags in an XML Node</summary>
        STATIC METHOD FindTag(nId AS INT64) AS ARRAY
            LOCAL oNode AS XmlNode
            oNode := FindNode(nId, OUT VAR oDoc)
            IF oNode != NULL
                RETURN AsXmlArray(oNode, oDoc)
            ENDIF
            RETURN NULL_ARRAY

        /// <summary>Find a child tag in an XML Node</summary>
        STATIC METHOD FindChildTag(nId AS INT64, cChildName AS STRING, lFirst AS LOGIC) AS USUAL
            IF lFirst
                RETURN FindFirstChildTag(nId, cChildName)
            ELSE
                RETURN FindAllChildTags(nId, cChildName)
            ENDIF

        /// <summary>Find a child tag in an XML Node</summary>
        STATIC METHOD FindFirstChildTag(nId AS INT64, cChildName AS STRING) AS INT64
            LOCAL oNode AS XmlNode
            oNode := FindNode(nId, OUT VAR oDoc)
            IF oNode != NULL
                FOREACH oChild AS XmlNode IN oNode:ChildNodes
                    IF String.Compare(oChild:Name, cChildName, TRUE) == 0
                        IF oDoc:Nodes:ContainsKey(oChild)
                            RETURN oDoc:Nodes[oChild]
                        ENDIF
                    ENDIF
                NEXT
            ENDIF
            // When not found we return -1
            RETURN -1

        STATIC METHOD FindAllChildTags(nId AS INT64, cChildName AS STRING) AS ARRAY
            LOCAL oNode AS XmlNode
            // When lFirst = TRUE then  return the id of that node or -1
            // When lFirst = FALSE then return an array of nodes
            oNode := FindNode(nId, OUT VAR oDoc)
            IF oNode != NULL
                LOCAL aResult := {} AS ARRAY
                FOREACH oChild AS XmlNode IN oNode:ChildNodes
                    IF String.Compare(oChild:Name, cChildName, TRUE) == 0
                        IF oDoc:Nodes:ContainsKey(oChild)
                            AAdd(aResult, oDoc:Nodes[oChild])
                        ENDIF
                    ENDIF
                NEXT
                IF ALen(aResult) > 0
                    RETURN aResult
                ENDIF
            ENDIF
            RETURN NULL_ARRAY
    /// <summary>Return the children of a node as a XPP specific XML array</summary>

        STATIC METHOD AsXmlArray(oNode AS XmlNode, oFoundDoc AS XDocument) AS ARRAY
            LOCAL aResult AS ARRAY
            LOCAL aChildren AS ARRAY
            LOCAL aAttributes AS ARRAY
            aChildren := {}
            IF oNode:HasChildNodes
                FOREACH oChild AS XmlNode IN oNode:ChildNodes
                    IF oFoundDoc:Nodes:ContainsKey(oChild)
                        AAdd(aChildren,oFoundDoc:Nodes[oChild])
                    ENDIF
                NEXT
            ENDIF
            aAttributes := {}
            IF oNode:Attributes != NULL
                FOREACH oAttr AS XmlAttribute IN oNode:Attributes
                    AAdd(aAttributes , {oAttr:Name, oAttr:Value, oAttr})
                NEXT
            ENDIF
            aResult := ArrayNew(XMLTAG_N_MEMBER)
            aResult [XMLTAG_NAME   ] := oNode:Name
            aResult [XMLTAG_CONTENT] := oNode:InnerXml
            aResult [XMLTAG_CHILD  ] := IIF(ALen(aChildren) == 0, NIL, aChildren)
            aResult [XMLTAG_ACTION ] := NIL
            aResult [XMLTAG_ATTRIB ] := IIF(ALen(aAttributes) == 0, NIL, aAttributes)
            aResult [XMLTAG_OBJECT ] := oNode
            RETURN aResult

        /// <summary></summary>
       STATIC METHOD RegisterError(nError AS USUAL) AS VOID
            RETURN

         /// <summary>Close all open document</summary>
       STATIC METHOD CloseAllDocuments() AS LOGIC
            Initialize()
            RETURN TRUE

         /// <summary>Clear all errors</summary>
       STATIC METHOD ClearAllErrors() AS LOGIC
            aErrors:Clear()
            RETURN TRUE

#endregion
        /// <summary>Unique handle for the document.</summary>
        INTERNAL  PROPERTY DocHandle AS INT64 AUTO
        INTERNAL PROPERTY Nodes     AS Dictionary<XmlNode, INT64> AUTO
        INTERNAL PROPERTY NodeIds   AS Dictionary<INT64, XmlNode> AUTO
        INTERNAL PROPERTY RootId    AS INT64 AUTO
        INTERNAL PROPERTY LastError AS INT64 AUTO
        PROTECT  oDoc 	   AS XmlDocument
        PROTECT  cFileName AS STRING
        PROTECT  Actions   AS List<XAction>
        CONSTRUCTOR
            NodeIds    := Dictionary<INT64, XmlNode>{}
            Nodes      := Dictionary<XmlNode, INT64>{}
            Actions    := List<XAction>{}
            DocHandle  := 0
            LastError  := 0
            RETURN

        PRIVATE METHOD AddError(e AS Exception) AS INT64
            LOCAL oError        := XError{cFileName, XMLDOC_ERROR_PROCESS} AS XError
            oError:Additional   := e:Message
            oError:DocHandle    := SELF:DocHandle
            VAR id              := NewHandle()
            aErrors:Add( oError)
            RETURN id

        PRIVATE METHOD AddOpenError(e AS XmlException) AS INT64
            LOCAL oError    := XError{cFileName, XMLDOC_ERROR_INVALID_XML} AS XError
            oError:Line     := e:LineNumber
            oError:Column   := e:LinePosition
            oError:Additional := e:Message
            oError:DocHandle := SELF:DocHandle
            IF e:Message:ToLower():Contains("does not match")
                oError:Id := XMLDOC_ERROR_ENDTAG_MISSING
            ENDIF
            aErrors:Add(oError)
            RETURN oError:Handle


        PRIVATE METHOD AddOpenError(oError AS XError) AS INT64
            aErrors:Add(oError)
            RETURN oError:Handle


        PRIVATE METHOD ClearError() AS VOID
            LastError  := 0

        private method Read(oReader as System.IO.TextReader) as logic
            LOCAL lOk := FALSE AS LOGIC
            TRY
                oDoc := XmlDocument{}
                oDoc:Load(oReader)
                DocHandle := NewHandle()
                BEGIN LOCK gate
                    aDocuments:Add(SELF)
                END LOCK
                lOk := TRUE
            CATCH e AS XmlException
                LastError := SELF:AddOpenError(e)
            CATCH e AS Exception
                LastError := SELF:AddError(e)
                BEGIN LOCK gate
                    IF aDocuments:Contains(SELF)
                        aDocuments:Remove(SELF)
                    ENDIF
                END LOCK
                lOk := FALSE
            end try
            RETURN lOk

         /// <summary>Open an XML File</summary>
        METHOD OpenFile(cFile AS STRING) AS LOGIC
            LOCAL lOk := FALSE AS LOGIC
            SELF:ClearError()
            SELF:cFileName := cFile
            IF String.IsNullOrEmpty(cFile)
                LastError := SELF:AddOpenError(XError{cFile, XMLDOC_ERROR_NO_FILENAME})
                RETURN FALSE
            ENDIF
            IF !System.IO.File.Exists(cFile)
                LastError := SELF:AddOpenError(XError{cFile, XMLDOC_ERROR_FILE_NOT_FOUND})
                RETURN FALSE
            ENDIF
            TRY
                BEGIN USING VAR oReader := System.IO.StreamReader{cFile,TRUE}
                    lOk := SELF:Read(oReader)
                    oReader:Close()
                END USING
                SELF:Process()
            CATCH e AS Exception
                LastError := SELF:AddError(e)
                lOk := FALSE
            END TRY
            RETURN lOk

        /// <summary>Open a XML document from a string</summary>
        METHOD OpenText(cContents AS STRING) AS LOGIC
            LOCAL lOk := FALSE AS LOGIC
            SELF:ClearError()
            TRY
                BEGIN USING VAR oReader := System.IO.StringReader{cContents}
                    lOk := SELF:Read(oReader)
                    oReader:Close()
                END USING
                SELF:Process()
            CATCH
                lOk := FALSE
            END TRY
            RETURN lOk

        /// <summary>CLose an opened XML document</summary>

        METHOD Close() AS LOGIC
            LOCAL lOk := FALSE AS LOGIC
            IF aDocuments:Contains(SELF)
                aDocuments:Remove(SELF)
                lOk := TRUE
            ENDIF
            RETURN lOk


        /// <summary>Process an opened XML document</summary>
        METHOD Process() AS VOID
            TRY
                SELF:NodeIds:Clear()
                SELF:Nodes:Clear()
                SELF:WalkNode(oDoc)
                VAR root := oDoc:DocumentElement
                SELF:RootId := Nodes[root]
            CATCH e AS XmlException
                LastError := SELF:AddOpenError(e)
            CATCH e AS Exception
                LastError := SELF:AddError(e)
            END TRY
            RETURN

        /// <summary>Loop through all the actions and process each node that matches the actions</summary>
        METHOD ProcessNodes() AS LOGIC
            LOCAL lOk := FALSE AS LOGIC
            TRY
                LOCAL root := oDoc:DocumentElement AS XmlNode
                FOREACH VAR action IN Actions
                    VAR nodes := root:SelectNodes(action:Name)
                    IF nodes:Count >0
                        FOREACH node AS XmlNode IN nodes
                            lOk := SELF:PerformActions(node, action)
                            IF ! lOk
                                RETURN FALSE
                            ENDIF
                        NEXT
                    ENDIF
                NEXT
                lOk := TRUE
            CATCH e AS XmlException
                LastError := SELF:AddOpenError(e)
                lOk := FALSE
            CATCH e AS Exception
                LastError := SELF:AddError(e)
                lOk := FALSE
            END TRY
            RETURN lOk

        /// <summary>Select Nodes via XPath selection </summary>
        METHOD SelectNodes(cSelect AS STRING) AS ARRAY
            LOCAL aResult AS ARRAY
            TRY
                LOCAL root := oDoc:DocumentElement AS XmlNode
                VAR nodes := root:SelectNodes(cSelect)
                aResult := {}
                IF nodes:Count >0
                    FOREACH node AS XmlNode IN nodes
                        AAdd(aResult, SELF:Nodes[node])
                    NEXT
                ENDIF
            catch
                RETURN NULL_ARRAY
            END TRY
            RETURN aResult

        /// <summary>Perform actions on an XML node.</summary>
        METHOD PerformActions(oNode AS XmlNode, oAction AS XAction) AS LOGIC
            LOCAL bBlock        AS CODEBLOCK
            LOCAL nTag          AS INT64
            LOCAL aAttributes   := NIL AS USUAL
            LOCAL nResult       AS USUAL
            LOCAL oWork         AS XmlNode
            LOCAL sb            AS StringBuilder
            bBlock := oAction:Block
            nTag   := SELF:Nodes[oNode]
            sb     := StringBuilder{oNode:Name}
            oWork  := oNode:ParentNode
            DO WHILE oWork != NULL .AND. ! oWork IS XmlDocument
                sb:Insert(0,oWork:Name+"/")
                oWork := oWork:ParentNode
            ENDDO
            sb:Insert(0, "/")
            IF oNode:Attributes != NULL
                aAttributes := {}
                FOREACH oAttr AS XmlAttribute IN oNode:Attributes
                    AAdd(aAttributes , {oAttr:Name, oAttr:Value})
                NEXT
            ENDIF
            nResult := Eval(bBlock, sb:ToString(), oNode:InnerXml, aAttributes, nTag)
            if IsLong(nResult)
                IF nResult == XML_PROCESS_ABORT
                    RETURN FALSE
                ENDIF
            ELSE
                RETURN FALSE
            ENDIF
            RETURN TRUE


        /// <summary>Walk all nodes and add them to the collections on the document class</summary>
        METHOD WalkNode(oNode AS XmlNode) AS VOID
            LOCAL nID AS INT64
            IF !SELF:Nodes:ContainsKey(oNode)
                nID := NewHandle()
                Nodes:Add(oNode, nID)
                NodeIds:Add(nID, oNode)
            ENDIF
	        FOREACH oChild AS XmlNode IN oNode:ChildNodes
		        SELF:WalkNode(oChild)
	        NEXT
	        RETURN

        /// <summary>Clear all actions on a document</summary>
        METHOD ClearActions() AS VOID
            SELF:Actions:Clear()
            RETURN

        /// <summary>Set an action on a XML node</summary>
        METHOD SetAction(cNode AS STRING, bAction AS CODEBLOCK) AS LONG
            SELF:Actions:Add(XAction{cNode, bAction})
            LOCAL root := oDoc:DocumentElement AS XmlNode
            VAR nodes := root:SelectNodes(cNode)
            RETURN nodes:Count


	END CLASS





END NAMESPACE // XML
