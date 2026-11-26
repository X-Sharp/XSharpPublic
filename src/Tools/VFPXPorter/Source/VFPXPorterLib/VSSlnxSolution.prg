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
/// written. When appending to an existing .slnx, the projects read by <see cref="Load" /> keep
/// their Type and solution folder.
/// </remarks>
CLASS VSSlnxSolution INHERIT VSSolution

    /// <summary>
    /// Project type of .xsproj files, as written in the Type attribute.
    /// </summary>
    PUBLIC CONST XSharpProjectType := "aa6c8d78-22ff-423a-9c7c-5f2393824e04" AS STRING

    // Type and solution folder of the projects read from an existing .slnx, by normalized path
    PRIVATE _loadedTypes   AS Dictionary<STRING, STRING>
    PRIVATE _loadedFolders AS Dictionary<STRING, STRING>

    PUBLIC CONSTRUCTOR()
        SUPER()
        SELF:_loadedTypes   := Dictionary<STRING, STRING>{ StringComparer.OrdinalIgnoreCase }
        SELF:_loadedFolders := Dictionary<STRING, STRING>{ StringComparer.OrdinalIgnoreCase }
        RETURN

    PUBLIC OVERRIDE PROPERTY FileExtension AS STRING GET ".slnx"

    /// <summary>
    /// Write the .slnx file: one Project element per project, inside its solution folder if it was
    /// read from an existing solution in one.
    /// </summary>
    PUBLIC OVERRIDE METHOD Save( solutionPath AS STRING ) AS VOID
        VAR doc := XmlDocument{}
        VAR root := doc:CreateElement( "Solution" )
        doc:AppendChild( root )
        VAR folders := Dictionary<STRING, XmlElement>{ StringComparer.OrdinalIgnoreCase }
        FOREACH xsProj AS VSProject IN SELF:Projects
            VAR path := VSSlnxSolution.ToSlnxPath( SELF:GetProjectPath( xsProj ) )
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
            VAR projType := SELF:GetProjectType( path )
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
                VAR projName := Path.GetFileNameWithoutExtension( path )
                IF SELF:Projects:Any( { p => String.Compare( p:Name, projName, TRUE ) == 0 } )
                    LOOP
                ENDIF
                // Project proxy: name and path only (.slnx has no project GUIDs)
                VAR p := VSProject{ projName }
                p:RelativePath := path:Replace( "/", "\" )
                SELF:Projects:Add( p )
                IF prjNode:HasAttribute( "Type" )
                    SELF:_loadedTypes[path] := prjNode:GetAttribute( "Type" )
                ENDIF
                VAR folderNode := prjNode:ParentNode ASTYPE XmlElement
                IF folderNode != NULL .AND. folderNode:Name == "Folder"
                    SELF:_loadedFolders[path] := folderNode:GetAttribute( "Name" )
                ENDIF
            NEXT
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

    // Type read from the existing .slnx, else the X# type for .xsproj, else none (VS infers it)
    PRIVATE METHOD GetProjectType( path AS STRING ) AS STRING
        IF SELF:_loadedTypes:TryGetValue( path, OUT VAR known )
            RETURN known
        ENDIF
        IF path:EndsWith( ".xsproj", StringComparison.OrdinalIgnoreCase )
            RETURN VSSlnxSolution.XSharpProjectType
        ENDIF
        RETURN ""

END CLASS

END NAMESPACE
