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
        STATIC aDocuments as List<XDocument>
        STATIC aErrors    as List<XError>
        STATIC nextHandle as INT64
        STATIC gate       as OBJECT
        STATIC CONSTRUCTOR()
            Initialize()
            gate       := Object{}

        STATIC METHOD Initialize() AS VOID
            aDocuments := List<XDocument>{}
            aErrors    := List<XError>{}
            nextHandle := 1
            RETURN 


        STATIC METHOD GetDocument (nHandle as INT64) AS XDocument
            FOREACH var doc in aDocuments
                IF doc:DocHandle == nHandle
                    return doc
                ENDIF
            NEXT
            RETURN NULL

        STATIC METHOD NewHandle() AS INT64
            BEGIN LOCK Gate
                RETURN nextHandle++
            END LOCK

        STATIC METHOD FindError(nError as INT64) AS XError
            return aErrors:Where( {x => x:ID == nError}):FirstOrDefault()

        STATIC METHOD GetErrors() AS IEnumerable<XError>
            return aErrors

        STATIC METHOD GetErrorsForDocument(nDocHandle as Int64) AS IEnumerable<XError>
            return aErrors:Where ( { x=> x:DocHandle == nDocHandle })


       STATIC METHOD FindParent(nId as INT64) AS INT64
            local oNode as XMLNode
            local oDoc  as XDocument
            oNode := FindNode(nID, out oDoc)
            IF oNode != NULL
                oNode := oNode:ParentNode
                if oDoc:Nodes:ContainsKey(oNode)
                    return oDoc:Nodes[oNode]
                ENDIF
            ENDIF
            RETURN 0
                

        STATIC PRIVATE METHOD FindNode(nID AS INT64, oDocResult OUT XDocument) as XMLNode
            local oFoundNode := NULL as XMLNode
            oDocResult := NULL
            BEGIN LOCK Gate
                FOREACH oDoc as XDocument in aDocuments
                    if oDoc:NodeIds:ContainsKey(nID)
                        oFoundNode := oDoc:NodeIDs[nID]
                        oDocResult := oDoc
                        RETURN oFoundNode
                    ENDIF
                NEXT
            END LOCK
            RETURN NULL



        STATIC METHOD FindTag(nId as INT64) AS Array
            local oNode as XMLNode
            local oDoc  as XDocument
            oNode := FindNode(nID, out oDoc)
            IF oNode != NULL
                RETURN AsXmlArray(oNode, oDoc)
            ENDIF
            RETURN NULL_ARRAY

        STATIC METHOD FindChildTag(nId as INT64, cChildName AS STRING, lFirst as LOGIC) AS USUAL
            local oNode as XMLNode
            local oDoc  as XDocument
            oNode := FindNode(nID, out oDoc)
            IF oNode != NULL
                local aResult := {} as ARRAY
                FOREACH oChild as XmlNode in oNode:ChildNodes
                    IF String.Compare(oChild:Name, cChildName, TRUE) == 0
                        if oDoc:Nodes:ContainsKey(oChild)
                            IF lFirst
                                RETURN oDoc:Nodes[oChild]
                            ELSE
                                aadd(aResult, oDoc:Nodes[oChild])
                            ENDIF
                        endif
                    ENDIF
                NEXT
                IF alen(aResult) > 0
                    return aResult
                ENDIF
            ENDIF
            RETURN NULL_ARRAY


        STATIC METHOD AsXmlArray(oNode as XMLNode, oFoundDoc as XDocument) AS ARRAY
            local aResult as ARRAY
            local aChildren as ARRAY
            local aAttributes AS ARRAY
            aChildren := {}
            IF oNode:HasChildNodes
                FOREACH oChild as XmlNode in oNode:ChildNodes
                    if oFoundDoc:Nodes:ContainsKey(oChild)
                        aadd(aChildren,oFoundDoc:Nodes[oChild])
                    endif
                NEXT
            ENDIF
            aAttributes := {}
            IF oNode:Attributes != NULL
                FOREACH oAttr as XmlAttribute in oNode:Attributes
                    AADD(aAttributes , {oAttr:Name, oAttr:Value, oAttr})
                NEXT
            ENDIF
            aResult := ArrayNew(XMLTAG_N_MEMBER)
            aResult [XMLTAG_NAME   ] := oNode:Name
            aResult [XMLTAG_CONTENT] := oNode:InnerXml
            aResult [XMLTAG_CHILD  ] := iif(aLen(aChildren) == 0, NIL, aChildren)
            aResult [XMLTAG_ACTION ] := NIL
            aResult [XMLTAG_ATTRIB ] := iif(aLen(aAttributes) == 0, NIL, aAttributes)
            aResult [XMLTAG_OBJECT ] := oNode
            RETURN aResult

        STATIC METHOD RegisterError(nError as USUAL) AS VOID
            RETURN

        STATIC METHOD CloseAllDocuments() AS LOGIC
            Initialize()
            RETURN TRUE

        STATIC METHOD ClearAllErrors() AS LOGIC
            aErrors:Clear()
            RETURN TRUE
            
#endregion
        
        PROPERTY DocHandle as INT64 AUTO
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

        PRIVATE METHOD AddError(e as Exception) AS INT64
            LOCAL oError := XError{cFileName, XMLDOC_ERROR_PROCESS} as XError
            oError:Additional := e:Message
            oError:DocHandle := SELF:DocHandle
            var id     := NewHandle()
            aErrors:Add( oError)
            RETURN id

        PRIVATE METHOD AddOpenError(e as XmlException) AS INT64
            LOCAL oError := XError{cFileName, XMLDOC_ERROR_INVALID_XML} as XError
            oError:Line   := e:LineNumber
            oError:Column := e:LinePosition
            oError:Additional := e:Message
            oError:DocHandle := SELF:DocHandle
            if e:Message:ToLower():Contains("does not match")
                oError:ID := XMLDOC_ERROR_ENDTAG_MISSING
            endif
            aErrors:Add(oError)
            RETURN oError:Handle




        PRIVATE METHOD AddOpenError(oError as XError) AS INT64
            aErrors:Add(oError)
            RETURN oError:Handle


        PRIVATE METHOD ClearError() AS VOID
            LastError  := 0

        PRIVATE METHOD Read(oReader AS System.IO.TextReader) AS LOGIC 
            LOCAL lOk := FALSE as LOGIC
            TRY
                oDoc := XMLDocument{}
                oDoc:Load(oReader)
                DocHandle := NewHandle()
                BEGIN LOCK gate
                    aDocuments:Add(SELF)
                END LOCK
                lOk := TRUE
            CATCH e as XmlException
                LastError := AddOpenError(e)
            CATCH e as Exception
                LastError := AddError(e)
                BEGIN LOCK gate
                    IF aDocuments:Contains(SELF)
                        aDocuments:Remove(SELF)
                    ENDIF
                END LOCK
                lOk := FALSE
            END TRY            
            RETURN lOk

        METHOD OpenFile(cFile as STRING) AS LOGIC
            LOCAL lOk := FALSE as LOGIC
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
                BEGIN USING var oReader := System.IO.StreamReader{cFile,TRUE}
                    lOk := SELF:Read(oReader)
                    oReader:Close()
                END USING
                SELF:Process()
            CATCH e as Exception
                LastError := AddError(e)
                lOk := FALSE
            END TRY
            RETURN lOk
        
        METHOD OpenText(cContents AS STRING) AS LOGIC
            LOCAL lOk := FALSE as LOGIC
            SELF:ClearError()
            TRY
                BEGIN USING var oReader := System.IO.StringReader{cContents}
                    lOk := SELF:Read(oReader)
                    oReader:Close()
                END USING
                SELF:Process()
            CATCH
                lOk := FALSE
            END TRY
            RETURN lOk

        METHOD Close() AS LOGIC
            LOCAL lOk := FALSE as LOGIC
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
                var root := oDoc:DocumentElement
                SELF:RootId := Nodes[root]
            CATCH e as XmlException
                LastError := AddOpenError(e)
            CATCH e as Exception
                LastError := AddError(e)
            END TRY
            RETURN

        // loop through all the actions and process each node that matches the actions
        METHOD ProcessNodes() AS LOGIC
            LOCAL lOk := FALSE as LOGIC
            TRY
                local root := oDoc:DocumentElement as XmlNode
                foreach var action in actions
                    var nodes := root:SelectNodes(action:name)
                    if nodes:Count >0
                        FOREACH node as Xmlnode in nodes
                            lOk := SELF:PerformActions(node, action)
                            IF ! lOk
                                RETURN FALSE
                            ENDIF
                        NEXT
                    ENDIF
                next
                lOk := TRUE
            CATCH e as XmlException
                LastError := AddOpenError(e)
                lOk := FALSE
            CATCH e as Exception
                LastError := AddError(e)
                lOk := FALSE
            END TRY
            RETURN lOk


        METHOD PerformActions(oNode as XMLNode, oAction as XAction) AS LOGIC
            local bBlock        as CodeBlock
            local nTag          as INT64
            local aAttributes   := NIL as USUAL
            LOCAL nResult       as USUAL
            LOCAL oWork         as XMLNode
            LOCAL sb            as StringBuilder
            bBlock := oAction:Block
            nTag   := SELF:Nodes[oNode]
            sb     := StringBuilder{oNode:Name}
            oWork  := oNode:ParentNode
            DO WHILE oWork != NULL .and. ! oWork IS XmlDocument
                sb:Insert(0,oWork:Name+"/")
                oWork := oWork:ParentNode
            ENDDO
            sb:Insert(0, "/")
            IF oNode:Attributes != NULL
                aAttributes := {}
                FOREACH oAttr as XmlAttribute in oNode:Attributes
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
            LOCAL nID as INT64
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

        METHOD SetAction(cNode as STRING, bAction as CodeBlock) AS LONG
            SELF:Actions:Add(XAction{cNode, bAction})
            local root := oDoc:DocumentElement as XmlNode
            var nodes := root:SelectNodes(cNode)
            return nodes:Count


	END CLASS

    CLASS XError
        PROPERTY FileName as STRING AUTO
        PROPERTY Line     AS LONG AUTO
        PROPERTY Column   AS LONG AUTO
        PROPERTY Id       AS INT64 AUTO
        PROPERTY Additional as USUAL AUTO
        PROPERTY DocHandle as INT64 AUTO
        PROPERTY Handle    as INT64 AUTO
        CONSTRUCTOR(cFile as STRING, nId as INT64)
            SELF:FileName := cFile
            SELF:ID       := nID
            SELF:Additional := ""
            SELF:Handle := XDocument.NewHandle()

        METHOD ToArray() AS ARRAY
            var aResult := ArrayNew(XML_ERROR_ADDINFO) 
            aResult[XML_ERROR_ID]       := SELF:Id
            aResult[XML_ERROR_FILE]     := SELF:FileName
            aResult[XML_ERROR_LINE]     := SELF:Line
            aResult[XML_ERROR_COLUMN]   := SELF:Column
            aResult[XML_ERROR_ADDINFO]  := SELF:Additional
            RETURN aResult

    END CLASS

    CLASS XAction

        PROPERTY Name as STRING AUTO
        PROPERTY Block as CodeBlock AUTO
        CONSTRUCTOR(cName as STRING, oBlock as CodeBlock)
            Name := cName
            Block := oBlock
            RETURN
    END CLASS

END NAMESPACE // XML
