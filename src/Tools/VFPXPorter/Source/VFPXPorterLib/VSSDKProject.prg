// VSSDKProject.prg
// SDK-style .xsproj writer (<Project Sdk="Microsoft.NET.Sdk">)
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.

USING System
USING System.Collections.Generic
USING System.Text
USING System.Xml
USING System.IO

BEGIN NAMESPACE VFPXPorterLib

/// <summary>
/// Writes an exported project as an SDK-style .xsproj.
/// </summary>
/// <remarks>
/// Same model as <see cref="VSProject" /> (files, references, project references), so the solution,
/// XIDE export and XPorterProject handle it unchanged; only the file written by Save() differs.
/// The X# compiler is found through the X# installation (MSBuild ImportBefore hook), so the project
/// contains nothing X#-specific for the build.
/// </remarks>
CLASS VSSDKProject INHERIT VSProject

    /// <summary>
    /// Target framework moniker, written as-is (e.g. "net472", "net8.0-windows").
    /// </summary>
    PROPERTY TargetFramework AS STRING AUTO

    /// <summary>
    /// How the X# runtime is referenced. InstalledAssemblies is only possible for .NET Framework
    /// targets: the X# installation has no .NET 8 runtime assemblies.
    /// </summary>
    PROPERTY XSharpReferences AS XSharpReferenceMode AUTO

    /// <summary>
    /// Version written on the X# PackageReference (NuGet mode).
    /// </summary>
    PROPERTY XSharpPackageVersion AS STRING AUTO

    /// <summary>
    /// The X# NuGet package referenced in NuGet mode. XSharp.VFP brings XSharp.VFP.UI, and
    /// XSharp.RT / XSharp.Core through its dependencies (same as the X# FoxPro project template).
    /// </summary>
    PUBLIC CONST XSharpVfpPackage := "XSharp.VFP" AS STRING

    /// <summary>
    /// Item types that XSharp.SDK.Props fills from globs regardless of EnableDefaultItems.
    /// </summary>
    PUBLIC STATIC PROPERTY SdkGlobbedItemTypes AS STRING[] ;
        GET <STRING>{ "Compile", "None", "EmbeddedResource", "NativeResource", "VOBinary", "ApplicationDefinition" }

    PUBLIC CONSTRUCTOR( nme AS STRING, tfm AS STRING, refMode AS XSharpReferenceMode, packageVersion AS STRING )
        SUPER( nme )
        SELF:TargetFramework := IIF( String.IsNullOrWhiteSpace( tfm ), XPorterSettings.DefaultTargetFramework, tfm:Trim() )
        SELF:XSharpReferences := refMode
        SELF:XSharpPackageVersion := IIF( String.IsNullOrWhiteSpace( packageVersion ), "*", packageVersion:Trim() )
        RETURN

    /// <summary>
    /// TRUE when the target framework is a .NET Framework one (net472, net48, net481, ...).
    /// </summary>
    PUBLIC PROPERTY IsNetFramework AS LOGIC GET VSSDKProject.IsNetFrameworkTarget( SELF:TargetFramework )

    /// <summary>
    /// TRUE for a .NET Framework TFM: "net" followed by digits only (net472, net48).
    /// net8.0-windows, netstandard2.0, netcoreapp3.1 are not.
    /// </summary>
    PUBLIC STATIC METHOD IsNetFrameworkTarget( tfm AS STRING ) AS LOGIC
        IF String.IsNullOrEmpty( tfm ) .OR. tfm:Length < 4 .OR. !tfm:StartsWith( "net", StringComparison.OrdinalIgnoreCase )
            RETURN FALSE
        ENDIF
        FOREACH c AS CHAR IN tfm:Substring( 3 )
            IF !Char.IsDigit( c )
                RETURN FALSE
            ENDIF
        NEXT
        RETURN TRUE

    /// <summary>
    /// The reference mode actually used: InstalledAssemblies falls back to NuGet (with a Warning)
    /// for a non-.NET Framework target.
    /// </summary>
    PUBLIC PROPERTY EffectiveXSharpReferences AS XSharpReferenceMode
        GET
            IF SELF:XSharpReferences == XSharpReferenceMode.InstalledAssemblies .AND. !SELF:IsNetFramework
                RETURN XSharpReferenceMode.NuGet
            ENDIF
            RETURN SELF:XSharpReferences
        END GET
    END PROPERTY

    /// <summary>
    /// Framework assemblies are implicit in SDK projects (WinForms comes from UseWindowsForms).
    /// Only System.Data.DataSetExtensions, not implicit on .NET Framework, is added; the X# runtime
    /// is added here only for InstalledAssemblies (NuGet packages are written by Save).
    /// </summary>
    PUBLIC OVERRIDE METHOD AddStandardReferences() AS VOID
        IF SELF:IsNetFramework
            SELF:AddReference( "System.Data.DataSetExtensions" )
        ENDIF
        IF SELF:XSharpReferences == XSharpReferenceMode.InstalledAssemblies .AND. !SELF:IsNetFramework
            XPorterLogger.Instance:Warning( "VSSDKProject '" + SELF:Name + "': the X# installation has no assemblies for '" + ;
                SELF:TargetFramework + "'; using the " + VSSDKProject.XSharpVfpPackage + " NuGet package instead." )
        ENDIF
        IF SELF:EffectiveXSharpReferences == XSharpReferenceMode.InstalledAssemblies
            FOREACH VAR asm IN <STRING>{ "XSharp.Core", "XSharp.RT", "XSharp.RDD", "XSharp.Data", "XSharp.VFP", "XSharp.VFP.UI" }
                SELF:AddReference( asm, TRUE )
            NEXT
        ENDIF
        RETURN

    /// <summary>
    /// StandardDefs relative to the project folder, using $(MSBuildProjectDirectory), which is always
    /// defined in SDK projects.
    /// </summary>
    PUBLIC OVERRIDE METHOD FormatStandardDefs( headerFullPath AS STRING, projectFolder AS STRING ) AS STRING
        IF String.IsNullOrEmpty( headerFullPath )
            RETURN ""
        ENDIF
        RETURN "$(MSBuildProjectDirectory)\" + GetRelativePath( projectFolder, headerFullPath )

    /// <summary>
    /// Write the SDK-style project file.
    /// </summary>
    /// <param name="projectFileNameFullPath">The full path to the project file.</param>
    /// <param name="stdDef">The StandardDefs value (see <see cref="FormatStandardDefs" />).</param>
    PUBLIC OVERRIDE METHOD Save( projectFileNameFullPath AS STRING, stdDef AS STRING ) AS VOID
        SELF:XmlDoc := XmlDocument{}
        VAR xsProj := SELF:CreateElement( SELF:XmlDoc, "Project" )
        VAR sdkAttr := SELF:XmlDoc:CreateAttribute( "Sdk" )
        sdkAttr:Value := "Microsoft.NET.Sdk"
        xsProj:Attributes:Append( sdkAttr )
        //
        VAR props := SELF:CreateElement( xsProj, "PropertyGroup" )
        SELF:CreateElement( props, "Name", SELF:Name + "App" )
        SELF:CreateElement( props, "OutputType", SELF:GetOutputTypeName() )
        SELF:CreateElement( props, "TargetFramework", SELF:TargetFramework )
        SELF:CreateElement( props, "UseWindowsForms", "true" )
        SELF:CreateElement( props, "RootNamespace", SELF:Name )
        SELF:CreateElement( props, "AssemblyName", SELF:GetAssemblyName( SELF:Name ) )
        // VFPXPorter lists every file itself: globbing would also compile Backup\*.prg and the
        // library subfolders, and the explicit items would be duplicates (NETSDK1022).
        SELF:CreateElement( props, "EnableDefaultItems", "false" )
        SELF:CreateElement( props, "GenerateAssemblyInfo", "false" )
        SELF:CreateElement( props, "GenerateFullPaths", "true" )
        // Same X# switches as the classic format, without repeats; OutputName is classic-only
        VAR written := HashSet<STRING>{ StringComparer.OrdinalIgnoreCase }
        FOREACH VAR switch IN SELF:GetCompilerSwitches( SELF:Name )
            IF String.Compare( switch:Key, "OutputName", TRUE ) != 0 .AND. written:Add( switch:Key )
                SELF:CreateElement( props, switch:Key, switch:Value )
            ENDIF
        NEXT
        IF !String.IsNullOrEmpty( stdDef )
            SELF:CreateElement( props, "StandardDefs", stdDef )
        ENDIF
        SELF:CreateElement( props, "DefineConstants", "$(DefineConstants);VFP2XS" )
        SELF:CreateElement( props, "DisabledWarnings", "XS9025;XS9043" )
        //
        IF SELF:EffectiveXSharpReferences == XSharpReferenceMode.NuGet
            VAR packages := SELF:CreateElement( xsProj, "ItemGroup" )
            VAR pkg := SELF:CreateElement( packages, "PackageReference" )
            VAR incAttr := SELF:XmlDoc:CreateAttribute( "Include" )
            incAttr:Value := VSSDKProject.XSharpVfpPackage
            pkg:Attributes:Append( incAttr )
            VAR verAttr := SELF:XmlDoc:CreateAttribute( "Version" )
            verAttr:Value := SELF:XSharpPackageVersion
            pkg:Attributes:Append( verAttr )
        ENDIF
        //
        IF SELF:ReferenceList:Count > 0
            VAR assembliesFolder := Path.Combine( VSSDKProject.XSharpInstallFolderOrDefault, "Assemblies" )
            VAR references := SELF:CreateElement( xsProj, "ItemGroup" )
            FOREACH refer AS Reference IN SELF:ReferenceList
                refer:AppendToSdk( references, assembliesFolder )
            NEXT
        ENDIF
        // XSharp.SDK.Props adds **/*.designer.prg, *.resx, *.rc, headers and VO binaries even when
        // EnableDefaultItems is false: duplicates of the explicit items below (MSB3105), and files of
        // the library projects in subfolders. Remove everything it added before listing our files.
        VAR cleanup := SELF:CreateElement( xsProj, "ItemGroup" )
        VAR labelAttr := SELF:XmlDoc:CreateAttribute( "Label" )
        labelAttr:Value := "Remove the items added by the X# SDK props: VFPXPorter lists every file"
        cleanup:Attributes:Append( labelAttr )
        FOREACH VAR itemType IN VSSDKProject.SdkGlobbedItemTypes
            VAR removeNode := SELF:CreateElement( cleanup, itemType )
            VAR removeAttr := SELF:XmlDoc:CreateAttribute( "Remove" )
            removeAttr:Value := "@(" + itemType + ")"
            removeNode:Attributes:Append( removeAttr )
        NEXT
        // Compile / None items: same elements as the classic format
        IF SELF:FileList:Count > 0
            VAR files := SELF:CreateElement( xsProj, "ItemGroup" )
            FOREACH file AS AssetInfo IN SELF:FileList
                file:AppendTo( files )
            NEXT
        ENDIF
        //
        IF SELF:ProjectReferenceList:Count > 0
            VAR libs := SELF:CreateElement( xsProj, "ItemGroup" )
            FOREACH prj AS VSProject IN SELF:ProjectReferenceList
                VAR projRef := SELF:CreateElement( libs, "ProjectReference" )
                VAR projAttr := SELF:XmlDoc:CreateAttribute( "Include" )
                projAttr:Value := GetRelativePath( Path.GetDirectoryName( projectFileNameFullPath ), prj:ProjectFileNameFullPath )
                projRef:Attributes:Append( projAttr )
            NEXT
        ENDIF
        //
        SELF:ProjectFileNameFullPath := projectFileNameFullPath
        SELF:XmlDoc:Save( projectFileNameFullPath )
        RETURN

    /// <summary>
    /// X# installation folder, or the standard one when the registry has none.
    /// </summary>
    PRIVATE STATIC PROPERTY XSharpInstallFolderOrDefault AS STRING
        GET
            VAR folder := XPorterSettings.XSharpInstallFolder
            IF String.IsNullOrEmpty( folder )
                folder := "C:\Program Files (x86)\XSharp"
            ENDIF
            RETURN folder
        END GET
    END PROPERTY

END CLASS

END NAMESPACE
