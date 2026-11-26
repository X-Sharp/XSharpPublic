// VSProject.prg
// Created by    : fabri
// Creation Date : 11/15/2019 9:32:53 AM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.Xml
USING System.IO

BEGIN NAMESPACE VFPXPorterLib

/// <summary>
/// The VSProject class.
/// </summary>
CLASS VSProject

    PRIVATE existingFiles AS HashSet<STRING>

    PROPERTY FileList AS List<AssetInfo> AUTO

    PROPERTY ReferenceList AS List<Reference> AUTO

    PROPERTY ProjectReferenceList AS List<VSProject> AUTO

    PROPERTY Name AS STRING AUTO

    PROPERTY IsLibrary AS LOGIC
        GET
            RETURN SELF:ProjectType == ProjectType.ClassLibrary
        END GET
        SET
            IF value
                SELF:ProjectType := ProjectType.ClassLibrary
            ELSE
                SELF:ProjectType := ProjectType.WindowsExe
            ENDIF
        END SET
    END PROPERTY

    /// <summary>
    /// Gets or sets the type of Visual Studio project to generate.
    /// </summary>
    /// <remarks>
    /// This value controls how the project is emitted in the generated
    /// solution and project files. Typical values include
    /// <see cref="ProjectType.ClassLibrary" /> and
    /// <see cref="ProjectType.WindowsExe" />.
    /// </remarks>
    PROPERTY ProjectType AS ProjectType AUTO

    PROPERTY GUID AS STRING AUTO

    /// <summary>
    /// Gets or sets the path to the project file relative to the solution directory.
    /// </summary>
    /// <remarks>
    /// This value is used when generating the solution so that project entries
    /// reference the project file using a stable relative path.
    /// </remarks>
    PROPERTY RelativePath AS STRING AUTO
    PROPERTY ProjectFileNameFullPath AS STRING AUTO

    PROPERTY FrameworkVersion AS STRING AUTO
    PROPERTY XmlDoc as XmlDocument AUTO

    /// <summary>
    /// Default Constructor, Initialize properties
    /// </summary>
    PUBLIC CONSTRUCTOR( )
        SELF:FileList := List<AssetInfo>{}
        SELF:existingFiles := HashSet<STRING>{}
        SELF:ReferenceList := List<Reference>{}
        SELF:GUID := System.Guid.NewGuid().ToString("B"):ToUpper()
        SELF:ProjectReferenceList := List<VSProject>{}
        SELF:IsLibrary := FALSE
        SELF:ProjectType := ProjectType.WindowsExe
        // Default value; XPorterProject.CreateVSProject() sets it from XPorterSettings.FrameworkVersion
        SELF:FrameworkVersion := XPorterSettings.DefaultFrameworkVersion
        RETURN

    /// <summary>
    /// Create a VSProject, giving its name
    /// </summary>
    /// <param name="nme"></param>
    PUBLIC CONSTRUCTOR( nme AS STRING )
        SELF()	// Constructor Chaining
        SELF:Name := nme
        //
        RETURN

    PUBLIC METHOD AddFile( filePath AS STRING,  faAction AS FileAction ) AS VOID
        IF SELF:existingFiles:Add( filePath )
            SELF:FileList:Add( AssetInfo{ filePath, faAction } )
        ELSE
            XPorterLogger.Instance:Warning( "Duplicate file : " + filePath )
        ENDIF

    PUBLIC METHOD AddFile( filePath AS STRING ) AS VOID
        IF SELF:existingFiles:Add( filePath )
            SELF:FileList:Add( FileInfo{ filePath} )
        ELSE
            XPorterLogger.Instance:Warning( "Duplicate file : " + filePath )
        ENDIF

    PUBLIC METHOD AddFile( filePath AS STRING, fileType AS STRING ) AS VOID
        IF SELF:existingFiles:Add( filePath )
            SELF:FileList:Add( FileInfo{ filePath, fileType } )
        ELSE
            XPorterLogger.Instance:Warning( "Duplicate file : " + filePath )
        ENDIF

    PUBLIC METHOD AddFile( filePath AS STRING, fileType AS STRING, depends AS STRING ) AS VOID
        IF SELF:existingFiles:Add( filePath )
            SELF:FileList:Add( FileInfo{ filePath, fileType, depends } )
        ELSE
            XPorterLogger.Instance:Warning( "Duplicate file : " + filePath )
        ENDIF

    PUBLIC METHOD AddReference( refer AS STRING ) AS VOID
        SELF:ReferenceList:Add( Reference{ refer } )

    PUBLIC METHOD AddReference( refer AS STRING, isXSharp AS LOGIC ) AS VOID
        SELF:ReferenceList:Add( Reference{ refer, isXSharp } )

    PUBLIC METHOD AddReference( refer AS Reference ) AS VOID
        SELF:ReferenceList:Add( refer )

    /// <summary>
    /// Add the references every exported project needs: the .NET Framework assemblies used by the
    /// generated code, and the X# runtime (including XSharp.VFP.UI).
    /// </summary>
    /// <remarks>Overridden by project formats that reference these differently (e.g. NuGet packages).</remarks>
    PUBLIC VIRTUAL METHOD AddStandardReferences() AS VOID
        SELF:AddReference( "mscorlib" )
        SELF:AddReference( "System" )
        SELF:AddReference( "System.Core" )
        SELF:AddReference( "System.Data" )
        SELF:AddReference( "System.Data.DataSetExtensions" )
        SELF:AddReference( "System.Xml" )
        SELF:AddReference( "System.Xml.Linq" )
        SELF:AddReference( "System.Drawing" )
        SELF:AddReference( "System.Windows.Forms" )
        SELF:AddReference( "XSharp.Core", TRUE )
        SELF:AddReference( "XSharp.RT", TRUE )
        SELF:AddReference( "XSharp.RDD", TRUE )
        SELF:AddReference( "XSharp.Data", TRUE )
        SELF:AddReference( "XSharp.VFP", TRUE )
        // XSharp.VFP.UI is no longer copied from Data/Templates/Tools, so it MUST be referenced here
        SELF:AddReference( "XSharp.VFP.UI", TRUE )
        RETURN

    /// <summary>
    /// Build the value of the StandardDefs property, pointing to the VFPXPorter header file.
    /// </summary>
    /// <param name="headerFullPath">Full path of the VFPXPorter.xh header (empty if none).</param>
    /// <param name="projectFolder">Folder of the project file; the header path is made relative to it.</param>
    /// <returns>The StandardDefs value, or an empty string when there is no header.</returns>
    PUBLIC VIRTUAL METHOD FormatStandardDefs( headerFullPath AS STRING, projectFolder AS STRING ) AS STRING
        IF String.IsNullOrEmpty( headerFullPath )
            RETURN ""
        ENDIF
        RETURN "$(projectdir)" + GetRelativePath( projectFolder, headerFullPath )

    /// <summary>
    /// Save the XML document to the given path, using the project name and the standard definitions
    /// </summary>
    /// <param name="projectFileNameFullPath">The full path to the project file.</param>
    /// <param name="stdDef">The standard definitions to use.</param>
    PUBLIC VIRTUAL METHOD Save( projectFileNameFullPath AS STRING, stdDef AS STRING ) AS VOID
        XmlDoc := XmlDocument{}
        //
        VAR xsProj := SELF:CreateElement(XmlDoc,"Project")
        LOCAL projAttr AS XmlAttribute
        projAttr := XmlDoc:CreateAttribute( "ToolsVersion" )
        projAttr:Value := "4.0"
        xsProj:Attributes:Append( projAttr )
        projAttr := XmlDoc:CreateAttribute( "DefaultTargets" )
        projAttr:Value := "Build"
        xsProj:Attributes:Append( projAttr )
        projAttr := XmlDoc:CreateAttribute( "xmlns" )
        projAttr:Value := "http://schemas.microsoft.com/developer/msbuild/2003"
        xsProj:Attributes:Append( projAttr )
        //

        VAR import := SELF:CreateElement(xsProj, "Import" )
        LOCAL configAttr AS XmlAttribute
        configAttr := XmlDoc:CreateAttribute( "Project" )
        configAttr:Value := "$(XSharpMsBuildDir)\XSharp.Default.props"
        import:Attributes:Append( configAttr )
        //
        // Todo Should we generate a relative path ??
        //
        VAR config := SELF:CreateElement(xsProj, "PropertyGroup")
        SELF:AddCompileSettings( config, SELF:Name, stdDef )

        VAR references := SELF:CreateElement(xsProj, "ItemGroup")
        FOREACH refer AS Reference IN ReferenceList
            refer:AppendTo( references )
        NEXT
        //
        VAR files := SELF:CreateElement(xsProj, "ItemGroup")
        FOREACH file AS AssetInfo IN FileList
            file:AppendTo( files )
        NEXT
        //


        config := SELF:CreateElement(xsProj, "PropertyGroup")
        SELF:AddConfigDebug( config )

        config := SELF:CreateElement(xsProj, "PropertyGroup")
        SELF:AddConfigRelease( config )

        //
        import := SELF:CreateElement( xsProj, "Import" )
        configAttr := XmlDoc:CreateAttribute( "Project" )
        configAttr:Value := "$(XSharpMsBuildDir)\XSharp.targets"
        import:Attributes:Append( configAttr )
        //
        IF SELF:ProjectReferenceList:Count > 0
            VAR libs := SELF:CreateElement(xsProj, "ItemGroup")
            FOREACH prj AS VSProject IN SELF:ProjectReferenceList
                import := SELF:CreateElement( libs, "ProjectReference" )
                configAttr := XmlDoc:CreateAttribute( "Include" )
                // Compute relative path from this project's folder to the referenced project file,
                // so references work correctly when projects live in different subfolders.
                configAttr:Value := GetRelativePath( Path.GetDirectoryName(projectFileNameFullPath), prj:ProjectFileNameFullPath )
                import:Attributes:Append( configAttr )
                SELF:CreateElement(import, "Name", prj:Name)
                SELF:CreateElement(import, "Project", prj:GUID)
                SELF:CreateElement(import, "Private", "True")
                //
            NEXT
        ENDIF
        //
        SELF:ProjectFileNameFullPath := projectFileNameFullPath
        SELF:XmlDoc:Save( projectFileNameFullPath )
        RETURN


    PROTECTED METHOD AddConfigCommon(parent AS XmlElement) AS VOID
        SELF:CreateElement( parent, "PlatformTarget","AnyCPU")
        SELF:CreateElement( parent, "ErrorReport", "prompt")
        SELF:CreateElement( parent, "WarningLevel", "4")
        SELF:CreateElement( parent, "DisabledWarnings",  "XS9025;XS9043")
        SELF:CreateElement(parent, "OutputPath", "bin\$(Configuration)\")


    PROTECTED METHOD AddConfigRelease(parent AS XmlElement ) AS VOID
        LOCAL configAttr AS XmlAttribute
        configAttr := XmlDoc:CreateAttribute( "Condition" )
        configAttr:Value := "'$(Configuration)|$(Platform)' == 'Release|AnyCPU'"
        parent:Attributes:Append( configAttr )
        //
        SELF:CreateElement(parent, "EmitDebugInformation","False")
        SELF:CreateElement(parent, "DebugType","pdbonly")
        SELF:CreateElement(parent, "Optimize", "True")
        SELF:CreateElement(parent, "DefineConstants","VFP2XS;TRACE")
        SELF:AddConfigCommon(parent)


    PROTECTED METHOD AddConfigDebug(parent AS XmlElement ) AS VOID
        LOCAL configAttr AS XmlAttribute
        configAttr := XmlDoc:CreateAttribute( "Condition" )
        configAttr:Value := "'$(Configuration)|$(Platform)' == 'Debug|AnyCPU'"
        parent:Attributes:Append( configAttr )
        //
        SELF:CreateElement(parent, "EmitDebugInformation","True")
        SELF:CreateElement(parent, "DebugType","full")
        SELF:CreateElement(parent, "Optimize", "false")
        SELF:CreateElement(parent, "DefineConstants","VFP2XS;DEBUG;TRACE")
        SELF:AddConfigCommon(parent)

    PROTECTED METHOD CreateElement(parent AS XmlNode, name as STRING, strValue := NULL as STRING) AS XmlElement
        LOCAL node AS XmlElement
        node := SELF:XmlDoc:CreateElement(name)
        if ! String.IsNullOrEmpty(strValue)
            node:InnerText := strValue
        endif
        parent:AppendChild( node )
        RETURN node
    PROTECTED METHOD AddCompileSettings(parent AS XmlElement, name AS STRING, stdDef AS STRING ) AS VOID
        //dummy := xmlDoc:CreateElement("XSharpProjectExtensionsPath")
        //dummy:InnerText := "$(MSBuildExtensionsPath)\XSharp\"
        //parent:AppendChild( dummy )
        SELF:CreateElement(parent, "Name", name + "App")
        SELF:CreateElement(parent, "ProjectGuid", SELF:GUID)
        SELF:CreateElement(parent, "OutputType", SELF:GetOutputTypeName())

        SELF:CreateElement(parent, "AppDesignerFolder","Properties")
        SELF:CreateElement(parent, "RootNamespace", name)

        SELF:CreateElement(parent, "AssemblyName", SELF:GetAssemblyName( name ))

        SELF:CreateElement(parent, "TargetFrameworkVersion", SELF:FrameworkVersion)
        SELF:CreateElement(parent, "NoLogo", "true")
        SELF:CreateElement(parent, "GenerateFullPaths", "true")
        FOREACH VAR switch IN SELF:GetCompilerSwitches( name )
            SELF:CreateElement(parent, switch:Key, switch:Value)
        NEXT
        IF !String.IsNullOrEmpty( stdDef )
            SELF:CreateElement( parent, "StandardDefs", stdDef)
        ENDIF

    /// <summary>
    /// The X# compiler settings of an exported project (dialect and compatibility switches), in the
    /// order they are written. Shared by every project format.
    /// </summary>
    /// <param name="name">Project name (used for OutputName).</param>
    /// <remarks>
    /// The classic format writes AllowOldStyleAssignments twice; it is kept so existing exports stay
    /// byte-identical. Writers that must not repeat a property should skip keys already written.
    /// </remarks>
    /// <summary>
    /// MSBuild OutputType for the <see cref="ProjectType" />: Library, Exe or WinExe.
    /// </summary>
    PROTECTED METHOD GetOutputTypeName() AS STRING
        SWITCH SELF:ProjectType
        CASE ProjectType.ClassLibrary
            RETURN "Library"
        CASE ProjectType.Console
            RETURN "Exe"
        OTHERWISE
            RETURN "WinExe"
        END SWITCH

    /// <summary>
    /// Assembly name for the <see cref="ProjectType" />: name + "Lib", "Console" or "App".
    /// </summary>
    PROTECTED METHOD GetAssemblyName( name AS STRING ) AS STRING
        SWITCH SELF:ProjectType
        CASE ProjectType.ClassLibrary
            RETURN name + "Lib"
        CASE ProjectType.Console
            RETURN name + "Console"
        OTHERWISE
            RETURN name + "App"
        END SWITCH

    PROTECTED VIRTUAL METHOD GetCompilerSwitches( name AS STRING ) AS List<KeyValuePair<STRING, STRING>>
        VAR switches := List<KeyValuePair<STRING, STRING>>{}
        switches:Add( KeyValuePair<STRING, STRING>{ "Dialect", "FoxPro" } )
        switches:Add( KeyValuePair<STRING, STRING>{ "AllowOldStyleAssignments", "True" } )
        switches:Add( KeyValuePair<STRING, STRING>{ "LB", "true" } )
        switches:Add( KeyValuePair<STRING, STRING>{ "OutputName", name } )
        switches:Add( KeyValuePair<STRING, STRING>{ "Undeclared", "true" } )
        switches:Add( KeyValuePair<STRING, STRING>{ "MemVar", "true" } )
        switches:Add( KeyValuePair<STRING, STRING>{ "AllowOldStyleAssignments", "true" } )
        switches:Add( KeyValuePair<STRING, STRING>{ "Allowdot", "true" } )
        switches:Add( KeyValuePair<STRING, STRING>{ "Vo9", "true" } )
        switches:Add( KeyValuePair<STRING, STRING>{ "Vo15", "true" } )
        switches:Add( KeyValuePair<STRING, STRING>{ "Ins", "true" } )
        RETURN switches

END CLASS


CLASS AssetInfo
    PROPERTY Path AS STRING AUTO

    PROPERTY Action AS FileAction AUTO

    CONSTRUCTOR()
        SELF:Path := ""
        SELF:Action := FileAction.DoNotCopy

    CONSTRUCTOR( filePath AS STRING )
        SELF:Path := filePath
        SELF:Action := FileAction.DoNotCopy

    CONSTRUCTOR( filePath AS STRING, faAction AS FileAction )
        SELF:Path := filePath
        SELF:Action := faAction

    VIRTUAL METHOD AppendTo( parent AS XmlElement ) AS VOID
        VAR xmlDoc := parent:OwnerDocument
        VAR fileChild := xmlDoc:CreateElement("None")
        parent:AppendChild(fileChild)
        VAR fileInfo := xmlDoc:CreateAttribute( "Include" )
        fileInfo:Value := SELF:Path
        fileChild:Attributes:Append( fileInfo )
        //
        IF SELF:Action != FileAction.DoNotCopy
            VAR subType := xmlDoc:CreateElement("CopyToOutputDirectory")
            IF SELF:Action == FileAction.CopyAlways
                subType:InnerText := "Always"
            ELSE
                subType:InnerText := "PreserveNewest"
            ENDIF
            //
            fileChild:AppendChild( subType )
        ENDIF

END CLASS


CLASS FileInfo INHERIT AssetInfo

    PROPERTY Type AS STRING AUTO

    PROPERTY DependentUpon AS STRING AUTO

    CONSTRUCTOR()
        SELF:Path := ""
        SELF:Type := ""
        SELF:DependentUpon := ""

    CONSTRUCTOR( filePath AS STRING )
        SELF:Path := filePath
        SELF:Type := ""
        SELF:DependentUpon := ""

    CONSTRUCTOR( filePath AS STRING, fileType AS STRING )
        SELF:Path := filePath
        SELF:Type := fileType
        SELF:DependentUpon := ""

    CONSTRUCTOR( filePath AS STRING, fileType AS STRING, depends AS STRING )
        SELF:Path := filePath
        SELF:Type := fileType
        SELF:DependentUpon := depends


    METHOD AppendTo( parent AS XmlElement ) AS VOID
        VAR xmlDoc := parent:OwnerDocument
        VAR fileChild := xmlDoc:CreateElement("Compile")
        parent:AppendChild(fileChild)
        VAR fileInfo := xmlDoc:CreateAttribute( "Include" )
        fileInfo:Value := SELF:Path
        fileChild:Attributes:Append( fileInfo )
        //
        IF ! String.IsNullOrEmpty( SELF:Type )
            VAR subType := xmlDoc:CreateElement("SubType")
            subType:InnerText := SELF:Type
            //
            fileChild:AppendChild( subType )
        ENDIF
        //
        IF ! String.IsNullOrEmpty( SELF:DependentUpon )
            VAR subType := xmlDoc:CreateElement("DependentUpon")
            subType:InnerText := SELF:DependentUpon
            //
            fileChild:AppendChild( subType )
        ENDIF


END CLASS


CLASS Reference

    PROPERTY Include AS STRING AUTO
    PROPERTY IsXSharp AS LOGIC AUTO
    PROPERTY IsLocal AS LOGIC AUTO

    CONSTRUCTOR()
        SELF:Include := ""
        SELF:IsXSharp := FALSE
        SELF:IsLocal := FALSE

    CONSTRUCTOR( inc AS STRING )
        SELF:Include := inc
        SELF:IsXSharp := FALSE
        SELF:IsLocal := FALSE

    CONSTRUCTOR( inc AS STRING, isxs AS LOGIC )
        SELF:Include := inc
        SELF:IsXSharp := isxs

    CONSTRUCTOR( inc AS STRING, isxs AS LOGIC, islcl AS LOGIC )
        SELF:Include := inc
        SELF:IsXSharp := isxs
        SELF:IsLocal := islcl

    METHOD AppendTo( parent AS XmlElement ) AS VOID
        VAR xmlDoc := parent:OwnerDocument
        VAR refer := xmlDoc:CreateElement("Reference")
        parent:AppendChild(refer)
        VAR referInfo := xmlDoc:CreateAttribute( "Include" )
        IF SELF:Include:EndsWith(".dll")
            referInfo:Value := Path.GetFileNameWithoutExtension(SELF:Include)
        ELSE
            referInfo:Value := Path.GetFileName(SELF:Include)
        ENDIF
        refer:Attributes:Append( referInfo )
        IF SELF:IsLocal .OR. SELF:IsXSharp
            //
            LOCAL name, dll AS STRING
            //
            IF SELF:Include:EndsWith(".dll")
                name := Path.GetFileNameWithoutExtension(SELF:Include)
                dll := Path.GetFileName(SELF:Include)
            ELSE
                name := SELF:Include
                dll := SELF:Include + ".dll"
            ENDIF
            LOCAL dummy AS XmlElement
            dummy := xmlDoc:CreateElement("Name")
            dummy:InnerText := name
            refer:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("AssemblyName")
            dummy:InnerText := dll
            refer:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("SpecificVersion")
            dummy:InnerText := "False"
            refer:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("Private")
            dummy:InnerText :="True"
            refer:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("HintPath")
            IF SELF:IsXSharp
                dummy:InnerText := "C:\Program Files (x86)\XSharp\Assemblies\" + name + ".dll"
            ELSE
                dummy:InnerText := SELF:Include
            ENDIF
            refer:AppendChild( dummy )
        ENDIF

    /// <summary>
    /// Write this reference for an SDK-style project: only Include, plus a HintPath for a local
    /// DLL or an X# assembly. Framework assemblies need nothing else in SDK projects.
    /// </summary>
    /// <param name="parent">The ItemGroup to add to.</param>
    /// <param name="xsharpAssembliesFolder">Folder of the installed X# assemblies (used when IsXSharp).</param>
    METHOD AppendToSdk( parent AS XmlElement, xsharpAssembliesFolder AS STRING ) AS VOID
        VAR xmlDoc := parent:OwnerDocument
        VAR refer := xmlDoc:CreateElement("Reference")
        parent:AppendChild(refer)
        VAR referInfo := xmlDoc:CreateAttribute( "Include" )
        IF SELF:Include:EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
            referInfo:Value := Path.GetFileNameWithoutExtension(SELF:Include)
        ELSE
            referInfo:Value := Path.GetFileName(SELF:Include)
        ENDIF
        refer:Attributes:Append( referInfo )
        IF SELF:IsXSharp .OR. SELF:IsLocal
            VAR hint := xmlDoc:CreateElement("HintPath")
            IF SELF:IsXSharp
                hint:InnerText := Path.Combine( xsharpAssembliesFolder, referInfo:Value + ".dll" )
            ELSE
                hint:InnerText := SELF:Include
            ENDIF
            refer:AppendChild( hint )
        ENDIF
END CLASS

END NAMESPACE
