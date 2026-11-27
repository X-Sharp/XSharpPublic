// VSSlnxSolution.prg
// XML solution file writer/reader (.slnx, Visual Studio 2022 17.13 and later)
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.IO
USING System.Linq
USING System.Text
USING System.Xml

BEGIN NAMESPACE VFPXPorterLib

/// <summary>
/// Writes the exported solution as an .slnx file.
/// </summary>
/// <remarks>
/// Independent of the project format: classic and SDK-style .xsproj files can both be listed.
/// Debug and Release are the default configurations of an .slnx, so no Configurations element is
/// written. When appending to an existing .slnx, unchanged projects read by <see cref="Load" /> keep
/// their Type and solution folder.
/// </remarks>
CLASS VSSlnxSolution INHERIT VSSolution

    /// <summary>
    /// Project type of classic .xsproj files, as written in the Type attribute.
    /// </summary>
    PUBLIC CONST XSharpProjectType := "aa6c8d78-22ff-423a-9c7c-5f2393824e04" AS STRING

    /// <summary>
    /// Project type of SDK-style .xsproj files, as written in the Type attribute.
    /// </summary>
    PUBLIC CONST SdkXSharpProjectType := "ab494dce-a11b-4583-851e-d4a86775b3f0" AS STRING

    // Projects loaded from an existing .slnx and their solution folders
    PRIVATE _loadedProjects AS HashSet<VSProject>
    PRIVATE _loadedFolders AS Dictionary<STRING, STRING>
    PRIVATE _loadedDocument AS XmlDocument

    PUBLIC CONSTRUCTOR()
        SUPER()
        SELF:_loadedProjects := HashSet<VSProject>{}
        SELF:_loadedFolders := Dictionary<STRING, STRING>{ StringComparer.OrdinalIgnoreCase }
        RETURN

    PUBLIC OVERRIDE PROPERTY FileExtension AS STRING GET ".slnx"

    /// <summary>
    /// Write the .slnx file: one Project element per project, inside its solution folder if it was
    /// read from an existing solution in one.
    /// </summary>
    PUBLIC OVERRIDE METHOD Save( solutionPath AS STRING ) AS VOID
        VAR doc := SELF:_loadedDocument
        LOCAL root AS XmlElement
        IF doc == NULL
            doc := XmlDocument{}
            root := doc:CreateElement( "Solution" )
            doc:AppendChild( root )
        ELSE
            root := doc:DocumentElement
        ENDIF
        VAR folders := Dictionary<STRING, XmlElement>{ StringComparer.OrdinalIgnoreCase }
        VAR existingPaths := HashSet<STRING>{ StringComparer.OrdinalIgnoreCase }
        IF SELF:_loadedDocument != NULL
            FOREACH prjNode AS XmlElement IN doc:SelectNodes( "//Project" )
                VAR existingPath := prjNode:GetAttribute( "Path" )
                IF !String.IsNullOrEmpty( existingPath )
                    existingPaths:Add( VSSlnxSolution.ToSlnxPath( existingPath ) )
                ENDIF
            NEXT
        ENDIF
        FOREACH xsProj AS VSProject IN SELF:Projects
            VAR path := VSSlnxSolution.ToSlnxPath( SELF:GetProjectPath( xsProj ) )
            IF SELF:_loadedDocument != NULL
                IF existingPaths:Contains( path )
                    IF !SELF:_loadedProjects:Contains( xsProj )
                        FOREACH prjNode AS XmlElement IN doc:SelectNodes( "//Project" )
                            IF String.Equals( VSSlnxSolution.ToSlnxPath( prjNode:GetAttribute( "Path" ) ), path, StringComparison.OrdinalIgnoreCase )
                                VAR projType := SELF:GetProjectType( xsProj, path )
                                IF String.IsNullOrEmpty( projType )
                                    prjNode:RemoveAttribute( "Type" )
                                ELSE
                                    prjNode:SetAttribute( "Type", projType )
                                ENDIF
                                EXIT
                            ENDIF
                        NEXT
                    ENDIF
                    LOOP
                ENDIF
                VAR prj := doc:CreateElement( "Project" )
                prj:SetAttribute( "Path", path )
                VAR projType := SELF:GetProjectType( xsProj, path )
                IF !String.IsNullOrEmpty( projType )
                    prj:SetAttribute( "Type", projType )
                ENDIF
                root:AppendChild( prj )
                existingPaths:Add( path )
                LOOP
            ENDIF
            LOCAL parent := root AS XmlElement
            IF SELF:_loadedFolders:TryGetValue( path, OUT VAR folderName )
                IF !folders:TryGetValue( folderName, OUT VAR folder )
                    folder := doc:CreateElement( "Folder" )
                    folder:SetAttribute( "Name", folderName )
                    root:AppendChild( folder )
                    folders:Add( folderName, folder )
                ENDIF
                parent := folder
            ENDIF
            VAR prj := doc:CreateElement( "Project" )
            prj:SetAttribute( "Path", path )
            VAR projType := SELF:GetProjectType( xsProj, path )
            IF !String.IsNullOrEmpty( projType )
                prj:SetAttribute( "Type", projType )
            ENDIF
            parent:AppendChild( prj )
        NEXT
        //
        VAR xmlSettings := XmlWriterSettings{}
        xmlSettings:Indent := TRUE
        xmlSettings:IndentChars := "  "
        xmlSettings:OmitXmlDeclaration := TRUE
        xmlSettings:Encoding := UTF8Encoding{ FALSE }
        BEGIN USING VAR writer := XmlWriter.Create( solutionPath, xmlSettings )
            doc:Save( writer )
        END USING
        RETURN

    /// <summary>
    /// Read the projects of an existing .slnx (used when appending to a solution).
    /// </summary>
    PUBLIC OVERRIDE METHOD Load( solutionPath AS STRING ) AS LOGIC
        IF !File.Exists( solutionPath )
            RETURN FALSE
        ENDIF
        TRY
            VAR doc := XmlDocument{}
            doc:Load( solutionPath )
            FOREACH prjNode AS XmlElement IN doc:SelectNodes( "//Project" )
                VAR path := prjNode:GetAttribute( "Path" )
                IF String.IsNullOrEmpty( path )
                    LOOP
                ENDIF
                path := VSSlnxSolution.ToSlnxPath( path )
                IF SELF:Projects:Any( { p => String.Equals( VSSlnxSolution.ToSlnxPath( SELF:GetProjectPath( p ) ), path, StringComparison.OrdinalIgnoreCase ) } )
                    LOOP
                ENDIF
                VAR projName := Path.GetFileNameWithoutExtension( path )
                // Project proxy: name and path only (.slnx has no project GUIDs)
                VAR p := VSProject{ projName }
                p:RelativePath := path:Replace( "/", "\" )
                SELF:Projects:Add( p )
                SELF:_loadedProjects:Add( p )
                VAR folderNode := prjNode:ParentNode ASTYPE XmlElement
                IF folderNode != NULL .AND. folderNode:Name == "Folder"
                    SELF:_loadedFolders[path] := folderNode:GetAttribute( "Name" )
                ENDIF
            NEXT
            SELF:_loadedDocument := doc
        CATCH e AS Exception
            XPorterLogger.Instance:Warning( "VSSlnxSolution: cannot read '" + solutionPath + "': " + e:Message )
            RETURN FALSE
        END TRY
        RETURN TRUE

    /// <summary>
    /// .slnx paths use forward slashes.
    /// </summary>
    PUBLIC STATIC METHOD ToSlnxPath( path AS STRING ) AS STRING
        RETURN path:Replace( "\", "/" )

    // Same rule as VSSolution.Save: RelativePath, or <Name>.xsproj next to the solution
    PRIVATE METHOD GetProjectPath( xsProj AS VSProject ) AS STRING
        RETURN IIF( String.IsNullOrEmpty( xsProj:RelativePath ), xsProj:Name + ".xsproj", xsProj:RelativePath )

    // Use the project format for new projects; unchanged loaded projects retain their XML Type.
    PRIVATE METHOD GetProjectType( xsProj AS VSProject, path AS STRING ) AS STRING
        IF xsProj IS VSSDKProject
            RETURN VSSlnxSolution.SdkXSharpProjectType
        ENDIF
        IF path:EndsWith( ".xsproj", StringComparison.OrdinalIgnoreCase )
            RETURN VSSlnxSolution.XSharpProjectType
        ENDIF
        RETURN ""

END CLASS

END NAMESPACE
