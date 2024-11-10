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

    PROPERTY IsLibrary AS LOGIC AUTO

    PROPERTY GUID AS STRING AUTO

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
        // TODO : set the Framework version as a Setting ??
        SELF:FrameworkVersion := "4.7.2"
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

    PUBLIC METHOD Save( projectFileNameFullPath AS STRING, stdDef AS STRING ) AS VOID
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
                configAttr:Value := prj:Name + ".xsproj"
                import:Attributes:Append( configAttr )
                SELF:CreateElement(import, "Name", prj:Name)
                SELF:CreateElement(import, "Project", prj:GUID)
                SELF:CreateElement(import, "Private", "True")
                //
            NEXT
        ENDIF
        //
        SELF:XmlDoc:Save( projectFileNameFullPath )
        RETURN


    PRIVATE METHOD AddConfigCommon(parent AS XmlElement) AS VOID
        SELF:CreateElement( parent, "PlatformTarget","AnyCPU")
        SELF:CreateElement( parent, "ErrorReport", "prompt")
        SELF:CreateElement( parent, "WarningLevel", "4")
        SELF:CreateElement( parent, "DisabledWarnings",  "XS9025;XS9043")
        SELF:CreateElement(parent, "OutputPath", "bin\$(Configuration)\")


    PRIVATE METHOD AddConfigRelease(parent AS XmlElement ) AS VOID
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


    PRIVATE METHOD AddConfigDebug(parent AS XmlElement ) AS VOID
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

    PRIVATE METHOD CreateElement(parent AS XmlNode, name as STRING, strValue := NULL as STRING) AS XmlElement
        LOCAL node AS XmlElement
        node := SELF:XmlDoc:CreateElement(name)
        if ! String.IsNullOrEmpty(strValue)
            node:InnerText := strValue
        endif
        parent:AppendChild( node )
        RETURN node
    PRIVATE METHOD AddCompileSettings(parent AS XmlElement, name AS STRING, stdDef AS STRING ) AS VOID
        //dummy := xmlDoc:CreateElement("XSharpProjectExtensionsPath")
        //dummy:InnerText := "$(MSBuildExtensionsPath)\XSharp\"
        //parent:AppendChild( dummy )
        SELF:CreateElement(parent, "Name", name + "App")
        SELF:CreateElement(parent, "ProjectGuid", SELF:GUID)
        SELF:CreateElement(parent, "OutputType", IIF(SELF:IsLibrary, "Library", "WinExe"))
        SELF:CreateElement(parent, "AppDesignerFolder","Properties")
        SELF:CreateElement(parent, "RootNamespace", name)
        SELF:CreateElement(parent, "AssemblyName", IIF(SELF:IsLibrary, name + "Lib", name + "App"))
        SELF:CreateElement(parent, "TargetFrameworkVersion", SELF:FrameworkVersion)
        SELF:CreateElement(parent, "NoLogo", "true")
        SELF:CreateElement(parent, "GenerateFullPaths", "true")
        SELF:CreateElement(parent, "Dialect", "FoxPro")
        SELF:CreateElement(parent, "AllowOldStyleAssignments", "True")
        SELF:CreateElement(parent, "LB", "true")
        SELF:CreateElement(parent, "OutputName", name)
        SELF:CreateElement(parent, "Undeclared", "true")
        SELF:CreateElement(parent, "MemVar", "true")
        SELF:CreateElement(parent, "AllowOldStyleAssignments", "true")
        SELF:CreateElement(parent, "Allowdot", "true")
        SELF:CreateElement(parent, "Vo9", "true")
        SELF:CreateElement(parent, "Vo15", "true")
        SELF:CreateElement(parent, "Ins", "true")
        IF !String.IsNullOrEmpty( stdDef )
            SELF:CreateElement( parent, "StandardDefs", stdDef)
        ENDIF

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
END CLASS

END NAMESPACE
