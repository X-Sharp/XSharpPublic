// XMLDocument.prg
// Created by    : robert
// Creation Date : 5/2/2019 10:37:00 AM
// Created for   : 
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Collections.Concurrent
USING System.Text
USING System.XML
USING System.Linq

BEGIN NAMESPACE XSharp.XPP

	CLASS XDocument
#region Static 
        STATIC aDocuments AS List<XDocument>
        STATIC aErrors    AS List<XError>
        STATIC nextHandle AS INT64
        STATIC gate       AS OBJECT
        STATIC CONSTRUCTOR()
            Initialize()
            gate       := OBJECT{}

        STATIC METHOD Initialize() AS VOID
            aDocuments := List<XDocument>{}
            aErrors    := List<XError>{}
            nextHandle := 1
            RETURN 


        STATIC METHOD GetDocument (nHandle AS INT64) AS XDocument
            FOREACH VAR doc IN aDocuments
                IF doc:DocHandle == nHandle
                    RETURN doc
                ENDIF
            NEXT
            RETURN NULL

        STATIC METHOD NewHandle() AS INT64
            BEGIN LOCK Gate
                RETURN nextHandle++
            END LOCK

        STATIC METHOD FindError(nError AS INT64) AS XError
            RETURN aErrors:Where( {x => x:ID == nError}):FirstOrDefault()

        STATIC METHOD GetErrors() AS IEnumerable<XError>
            RETURN aErrors

        STATIC METHOD GetErrorsForDocument(nDocHandle AS INT64) AS IEnumerable<XError>
            RETURN aErrors:Where ( { x=> x:DocHandle == nDocHandle })


       STATIC METHOD FindParent(nId AS INT64) AS INT64
            LOCAL oNode AS XMLNode
            LOCAL oDoc  AS XDocument
            oNode := FindNode(nID, OUT oDoc)
            IF oNode != NULL
                oNode := oNode:ParentNode
                IF oDoc:Nodes:ContainsKey(oNode)
                    RETURN oDoc:Nodes[oNode]
                ENDIF
            ENDIF
            RETURN 0
                

        STATIC PRIVATE METHOD FindNode(nID AS INT64, oDocResult OUT XDocument) AS XMLNode
            LOCAL oFoundNode := NULL AS XMLNode
            oDocResult := NULL
            BEGIN LOCK Gate
                FOREACH oDoc AS XDocument IN aDocuments
                    IF oDoc:NodeIds:ContainsKey(nID)
                        oFoundNode := oDoc:NodeIDs[nID]
                        oDocResult := oDoc
                        RETURN oFoundNode
                    ENDIF
                NEXT
            END LOCK
            RETURN NULL



        STATIC METHOD FindTag(nId AS INT64) AS ARRAY
            LOCAL oNode AS XMLNode
            LOCAL oDoc  AS XDocument
            oNode := FindNode(nID, OUT oDoc)
            IF oNode != NULL
                RETURN AsXmlArray(oNode, oDoc)
            ENDIF
            RETURN NULL_ARRAY

        STATIC METHOD FindChildTag(nId AS INT64, cChildName AS STRING, lFirst AS LOGIC) AS USUAL
            LOCAL oNode AS XMLNode
            LOCAL oDoc  AS XDocument
            // When lFirst = TRUE then  return the id of that node or -1
            // When lFirst = FALSE then return an array of nodes
            oNode := FindNode(nID, OUT oDoc)
            IF oNode != NULL
                LOCAL aResult := {} AS ARRAY
                FOREACH oChild AS XmlNode IN oNode:ChildNodes
                    IF String.Compare(oChild:Name, cChildName, TRUE) == 0
                        IF oDoc:Nodes:ContainsKey(oChild)
                            IF lFirst
                                RETURN oDoc:Nodes[oChild]
                            ELSE
                                aadd(aResult, oDoc:Nodes[oChild])
                            ENDIF
                        ENDIF
                    ENDIF
                NEXT
                IF alen(aResult) > 0
                    RETURN aResult
                ENDIF
            ENDIF
            IF lFirst
                RETURN 0
            ELSE
                RETURN NULL_ARRAY
            ENDIF


        STATIC METHOD AsXmlArray(oNode AS XMLNode, oFoundDoc AS XDocument) AS ARRAY
            LOCAL aResult AS ARRAY
            LOCAL aChildren AS ARRAY
            LOCAL aAttributes AS ARRAY
            aChildren := {}
            IF oNode:HasChildNodes
                FOREACH oChild AS XmlNode IN oNode:ChildNodes
                    IF oFoundDoc:Nodes:ContainsKey(oChild)
                        aadd(aChildren,oFoundDoc:Nodes[oChild])
                    ENDIF
                NEXT
            ENDIF
            aAttributes := {}
            IF oNode:Attributes != NULL
                FOREACH oAttr AS XmlAttribute IN oNode:Attributes
                    AADD(aAttributes , {oAttr:Name, oAttr:Value, oAttr})
                NEXT
            ENDIF
            aResult := ArrayNew(XMLTAG_N_MEMBER)
            aResult [XMLTAG_NAME   ] := oNode:Name
            aResult [XMLTAG_CONTENT] := oNode:InnerXml
            aResult [XMLTAG_CHILD  ] := IIF(aLen(aChildren) == 0, NIL, aChildren)
            aResult [XMLTAG_ACTION ] := NIL
            aResult [XMLTAG_ATTRIB ] := IIF(aLen(aAttributes) == 0, NIL, aAttributes)
            aResult [XMLTAG_OBJECT ] := oNode
            RETURN aResult

        STATIC METHOD RegisterError(nError AS USUAL) AS VOID
            RETURN

        STATIC METHOD CloseAllDocuments() AS LOGIC
            Initialize()
            RETURN TRUE

        STATIC METHOD ClearAllErrors() AS LOGIC
            aErrors:Clear()
            RETURN TRUE
            
#endregion
        
        PROPERTY DocHandle AS INT64 AUTO
        PROPERTY Nodes     AS Dictionary<XmlNode, INT64> AUTO
        PROPERTY NodeIds   AS Dictionary<INT64, XmlNode> AUTO
        PROPERTY RootId    AS INT64 AUTO
        PROPERTY LastError AS INT64 AUTO
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
            LOCAL oError := XError{cFileName, XMLDOC_ERROR_PROCESS} AS XError
            oError:Additional := e:Message
            oError:DocHandle := SELF:DocHandle
            VAR id     := NewHandle()
            aErrors:Add( oError)
            RETURN id

        PRIVATE METHOD AddOpenError(e AS XmlException) AS INT64
            LOCAL oError := XError{cFileName, XMLDOC_ERROR_INVALID_XML} AS XError
            oError:Line   := e:LineNumber
            oError:Column := e:LinePosition
            oError:Additional := e:Message
            oError:DocHandle := SELF:DocHandle
            IF e:Message:ToLower():Contains("does not match")
                oError:ID := XMLDOC_ERROR_ENDTAG_MISSING
            ENDIF
            aErrors:Add(oError)
            RETURN oError:Handle




        PRIVATE METHOD AddOpenError(oError AS XError) AS INT64
            aErrors:Add(oError)
            RETURN oError:Handle


        PRIVATE METHOD ClearError() AS VOID
            LastError  := 0

        PRIVATE METHOD Read(oReader AS System.IO.TextReader) AS LOGIC 
            LOCAL lOk := FALSE AS LOGIC
            TRY
                oDoc := XMLDocument{}
                oDoc:Load(oReader)
                DocHandle := NewHandle()
                BEGIN LOCK gate
                    aDocuments:Add(SELF)
                END LOCK
                lOk := TRUE
            CATCH e AS XmlException
                LastError := AddOpenError(e)
            CATCH e AS Exception
                LastError := AddError(e)
                BEGIN LOCK gate
                    IF aDocuments:Contains(SELF)
                        aDocuments:Remove(SELF)
                    ENDIF
                END LOCK
                lOk := FALSE
            END TRY            
            RETURN lOk

        METHOD OpenFile(cFile AS STRING) AS LOGIC
            LOCAL lOk := FALSE AS LOGIC
            SELF:ClearError()
            SELF:cFileName := cFile
            IF String.IsNullOrEmpty(cFile)
                LastError := AddOpenError(XError{cFile, XMLDOC_ERROR_NO_FILENAME})
                RETURN FALSE
            ENDIF
            IF !System.IO.File.Exists(cFile)
                LastError := AddOpenError(XError{cFile, XMLDOC_ERROR_FILE_NOT_FOUND})
                RETURN FALSE
            ENDIF
            TRY
                BEGIN USING VAR oReader := System.IO.StreamReader{cFile,TRUE}
                    lOk := SELF:Read(oReader)
                    oReader:Close()
                END USING
                SELF:Process()
            CATCH e AS Exception
                LastError := AddError(e)
                lOk := FALSE
            END TRY
            RETURN lOk
        
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

        METHOD Close() AS LOGIC
            LOCAL lOk := FALSE AS LOGIC
            IF aDocuments:Contains(SELF)
                aDocuments:Remove(SELF)
                lOk := TRUE
            ENDIF
            RETURN lOk
            

        METHOD Process() AS VOID
            TRY
                SELF:NodeIds:Clear()
                SELF:Nodes:Clear()
                SELF:WalkNode(oDoc)
                VAR root := oDoc:DocumentElement
                SELF:RootId := Nodes[root]
            CATCH e AS XmlException
                LastError := AddOpenError(e)
            CATCH e AS Exception
                LastError := AddError(e)
            END TRY
            RETURN

        // loop through all the actions and process each node that matches the actions
        METHOD ProcessNodes() AS LOGIC
            LOCAL lOk := FALSE AS LOGIC
            TRY
                LOCAL root := oDoc:DocumentElement AS XmlNode
                FOREACH VAR action IN actions
                    VAR nodes := root:SelectNodes(action:name)
                    IF nodes:Count >0
                        FOREACH node AS Xmlnode IN nodes
                            lOk := SELF:PerformActions(node, action)
                            IF ! lOk
                                RETURN FALSE
                            ENDIF
                        NEXT
                    ENDIF
                NEXT
                lOk := TRUE
            CATCH e AS XmlException
                LastError := AddOpenError(e)
                lOk := FALSE
            CATCH e AS Exception
                LastError := AddError(e)
                lOk := FALSE
            END TRY
            RETURN lOk

        // SelectNodes via cSelect
        METHOD SelectNodes(cSelect AS STRING) AS ARRAY
            LOCAL aResult AS ARRAY
            TRY
                LOCAL root := oDoc:DocumentElement AS XmlNode
                VAR nodes := root:SelectNodes(cSelect)
                aResult := {}
                IF nodes:Count >0
                    FOREACH node AS Xmlnode IN nodes
                        aAdd(aResult, SELF:Nodes[node])
                    NEXT
                ENDIF
            CATCH 
                RETURN NULL_ARRAY
            END TRY
            RETURN aResult


        METHOD PerformActions(oNode AS XMLNode, oAction AS XAction) AS LOGIC
            LOCAL bBlock        AS CODEBLOCK
            LOCAL nTag          AS INT64
            LOCAL aAttributes   := NIL AS USUAL
            LOCAL nResult       AS USUAL
            LOCAL oWork         AS XMLNode
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
                    AADD(aAttributes , {oAttr:Name, oAttr:Value})
                NEXT
            ENDIF
            nResult := eval(bBlock, sb:ToString(), oNode:InnerXml, aAttributes, nTag)
            IF IsLong(nResult) 
                IF nResult == XML_PROCESS_ABORT
                    RETURN FALSE
                ENDIF
            ELSE
                RETURN FALSE
            ENDIF
            RETURN TRUE
  

        // Wall all nodes and add them to the collections on the document class
        METHOD WalkNode(oNode AS XmlNode) AS VOID
            LOCAL nID AS INT64
            IF !SELF:Nodes:ContainsKey(oNode)
                nId := NewHandle()
                Nodes:Add(oNode, nID)
                NodeIDs:Add(nId, oNode)
            ENDIF
	        FOREACH oChild AS XmlNode IN oNode:ChildNodes
		        SELF:WalkNode(oChild)
	        NEXT
	        RETURN

        METHOD ClearActions() AS VOID
            SELF:Actions:Clear()
            RETURN

        METHOD SetAction(cNode AS STRING, bAction AS CODEBLOCK) AS LONG
            SELF:Actions:Add(XAction{cNode, bAction})
            LOCAL root := oDoc:DocumentElement AS XmlNode
            VAR nodes := root:SelectNodes(cNode)
            RETURN nodes:Count


	END CLASS

    CLASS XError
        PROPERTY FileName AS STRING AUTO
        PROPERTY Line     AS LONG AUTO
        PROPERTY Column   AS LONG AUTO
        PROPERTY Id       AS INT64 AUTO
        PROPERTY Additional AS USUAL AUTO
        PROPERTY DocHandle AS INT64 AUTO
        PROPERTY Handle    AS INT64 AUTO
        CONSTRUCTOR(cFile AS STRING, nId AS INT64)
            SELF:FileName := cFile
            SELF:ID       := nID
            SELF:Additional := ""
            SELF:Handle := XDocument.NewHandle()

        METHOD ToArray() AS ARRAY
            VAR aResult := ArrayNew(XML_ERROR_ADDINFO) 
            aResult[XML_ERROR_ID]       := SELF:Id
            aResult[XML_ERROR_FILE]     := SELF:FileName
            aResult[XML_ERROR_LINE]     := SELF:Line
            aResult[XML_ERROR_COLUMN]   := SELF:Column
            aResult[XML_ERROR_ADDINFO]  := SELF:Additional
            RETURN aResult

    END CLASS

    CLASS XAction

        PROPERTY Name AS STRING AUTO
        PROPERTY Block AS CODEBLOCK AUTO
        CONSTRUCTOR(cName AS STRING, oBlock AS CODEBLOCK)
            Name := cName
            Block := oBlock
            RETURN
    END CLASS

END NAMESPACE // XML
