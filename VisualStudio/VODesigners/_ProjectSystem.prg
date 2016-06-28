#using System.Collections.Generic
BEGIN NAMESPACE XSharp.ProjectAPI
    INTERFACE IVsProject
        METHOD AddFile(cItem as STRING) AS VOID    
        METHOD RemoveFile(cItem as STRING) AS VOID    
        METHOD IsFileInProject(cItem as STRING) AS LOGIC
        METHOD GetBinaries(cExt as String) AS List<String>
        METHOD OpenInEditor(cFile as STRING, nLine as LONG, nColumn as LONG) AS LOGIC
                
    END INTERFACE
    INTERFACE IVSSolution
        METHOD GetXSharpProjects() AS List<String>
        METHOD GetProject(cName as STRING) AS IVsProject
    END INTERFACE

    CLASS Projects
        
        STATIC PROTECT oSolution as IVSSolution
        STATIC METHOD RegisterSolution(loSolution as IVSSolution) AS VOID
            oSolution :=loSolution
            RETURN

        STATIC METHOD Find(cName as STRING)  as Module
            IF oSolution != NULL_OBJECT
                LOCAL oVsProject as IVsProject
                FOREACH VAR sProject in oSolution:GetXSharpProjects()
                    oVsProject := oSolution:GetProject(sProject)
                    IF oVsProject != NULL_OBJECT
                        IF oVsProject:IsFileInProject(cName)
                            RETURN Module{cName, Project{sProject, oVsProject}}
                        ENDIF                    
                    ENDIF
                NEXT
            ENDIF
            RETURN NULL
    END CLASS
    
    CLASS Project INHERIT Element
        PROTECT oProject as IVsProject
        CONSTRUCTOR (cFileName as STRING, loProject as IVsProject)
            SELF:Name := cFileName
            SELF:oProject := loProject
            RETURN

        METHOD AddItem(cItem as STRING) AS VOID
            IF oProject != NULL
                oProject:AddFile(cItem)
            ENDIF
            RETURN

        METHOD RemoveItem(cItem as STRING) AS VOID
            IF oProject != NULL
                oProject:RemoveFile(cItem)
            ENDIF
            RETURN

        METHOD ItemExists(cFileName as STRING) AS LOGIC
            IF oProject != NULL
                RETURN oProject:IsFileInProject(cFileName)
            ENDIF
            RETURN FALSE

        METHOD GetBinaries(nType as VOBinaryType, lParam as LOGIC) AS IList<VOBinary>
            LOCAL aFiles as List<String>
            LOCAL cExt as STRING
            LOCAL aResult as List<VOBinary>
            aResult := List<VOBinary>{}
            IF oProject != NULL
                // Get the XSharp Binaries
                cExt   := VOBinary.GetExtension(nType, FALSE)
                IF !String.IsNullOrEmpty(cExt)
                    aFiles := oProject:GetBinaries(cExt)
                    FOREACH VAR sFile in aFiles
                        aResult:Add(VOBinary{sFile})
                    NEXT
                ENDIF
                // Get the Vulcan Binaries
                cExt   := VOBinary.GetExtension(nType, TRUE)
                IF !String.IsNullOrEmpty(cExt)
                    aFiles := oProject:GetBinaries(cExt)
                    FOREACH VAR sFile in aFiles
                        aResult:Add(VOBinary{sFile})
                    NEXT
                ENDIF
            ENDIF
            RETURN aResult

        METHOD FindType(cFileName as STRING, lCaseSensitive as LOGIC) AS TypeInfo
            RETURN NULL_OBJECT

        METHOD OpenInEditor(oTypeInfo as TypeInfo) AS LOGIC
            RETURN FALSE

        METHOD OpenInEditor(oMember as @@Member) AS LOGIC
            RETURN FALSE

        METHOD OpenInEditor(cFile as STRING, nLine as LONG, nColumn as LONG) AS LOGIC
            IF SELF:oProject != NULL_OBJECT
                RETURN SELF:oProject:OpenInEditor(cFile, nLine, nColumn)
            ENDIF
            RETURN FALSE

        METHOD FindMember(cTypeName as STRING, cMemberName as STRING, lCaseSensitive as LOGIC) AS @@Member
            RETURN NULL_OBJECT

    END CLASS

    CLASS Element
        PROPERTY Name as STRING AUTO
    END CLASS

    CLASS VOBinary INHERIT Element
        PROPERTY Type as VOBinaryType AUTO

        CONSTRUCTOR(cFileName as STRING)
            SELF:Name := cFileName
            SELF:Type := DetermineType(cFileName)
            RETURN

        CONSTRUCTOR(cFileName as STRING, nType as VOBinaryType)
            SELF:Name := cFileName
            SELF:Type := nType
            RETURN

        STATIC METHOD DetermineType(cFileName as STRING) AS VOBinaryType
            LOCAL nType as VOBinaryType
            LOCAL cExt  as STRING
            cExt := System.IO.Path.GetExtension(cFileName):ToLower()
            SWITCH cExt
            CASE ".xsfrm"
                nType := VOBinaryType.Window
            CASE ".vnfrm"
                nType := VOBinaryType.Window
            CASE ".xsmnu"
                nType := VOBinaryType.Menu
            CASE ".vnmnu"
                nType := VOBinaryType.Menu
            CASE ".xsfs"
                nType := VOBinaryType.FieldSpec
            CASE ".vnfs"
                nType := VOBinaryType.FieldSpec
            CASE ".xsdbs"
                nType := VOBinaryType.DbServer
            CASE ".vndbs"
                nType := VOBinaryType.DbServer
            OTHERWISE
                nType := VOBinaryType.Unknown 
            END SWITCH
            RETURN nType

        STATIC METHOD GetExtension(nType as VOBinaryType, lVulcan as LOGIC) AS STRING
            LOCAL cExt as STRING
            SWITCH nType
            case VOBinaryType.FieldSpec
                if lVulcan
                    cExt := ".vnfs"
                else
                    cExt := ".xsfs"
                endif

            case VOBinaryType.Window
                if lVulcan
                    cExt := ".vnfrm"
                else
                    cExt := ".xsfrm"
                endif
            case VOBinaryType.Menu
                if lVulcan
                    cExt := ".vnmnu"
                else
                    cExt := ".xsmnu"
                endif
            case VOBinaryType.DbServer
                if lVulcan
                    cExt := ".vndbs"
                else
                    cExt := ".xsdbs"
                endif
            otherwise
                cExt := ""
            END SWITCH
            RETURN cExt

    END CLASS

    ENUM VOBinaryType
        Member Unknown := 0
        Member FieldSpec
        Member Window
        Member Menu
        Member DbServer
    END ENUM

    CLASS Module
        PROPERTY Parent     as Project  GET NULL_OBJECT
        PROPERTY SourceCode as STRING   GET String.Empty
        PROPERTY FileName   as STRING GET SET
        PROTECT oProject    as Project
        CONSTRUCTOR (sName as STRING, loProject as Project)
            SELF:FileName := sName
            SELF:oProject := loProject
            RETURN

        METHOD DeleteLines (nStart as INT, nCount as INT) AS LOGIC
            RETURN TRUE
        METHOD GetInfo(lManaged OUT LOGIC, lOpen OUT LOGIC, lDirty OUT LOGIC) AS LOGIC
            lManaged := lOpen := lDirty := FALSE
            RETURN TRUE
        METHOD GetLineCount() AS LONG
            RETURN 0
        METHOD InsertLine(nLine as LONG, sLine as STRING) AS LONG
            
            RETURN 0


        METHOD OpenInEditor() AS LOGIC
            RETURN OpenInEditor(1,1)

        METHOD OpenInEditor(nLine as LONG, nColumn as LONG) AS LOGIC
            IF SELF:Parent != NULL_OBJECT
                RETURN SELF:Parent:OpenInEditor(SELF:FileName, nLine, nColumn)
            ENDIF
            RETURN FALSE
    END CLASS
    

    CLASS TypeInfo
    END CLASS
    

    CLASS @@Member INHERIT Element
    END CLASS

    

END NAMESPACE
