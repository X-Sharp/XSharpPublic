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
            LOCAL xmlDoc AS XmlDocument
            xmlDoc := XmlDocument{}
            //
            VAR xsProj := xmlDoc:CreateElement("Project")
            LOCAL projAttr AS XmlAttribute
            projAttr := xmlDoc:CreateAttribute( "ToolsVersion" )
            projAttr:Value := "4.0"
            xsProj:Attributes:Append( projAttr )
            projAttr := xmlDoc:CreateAttribute( "DefaultTargets" )
            projAttr:Value := "Build"
            xsProj:Attributes:Append( projAttr )
            projAttr := xmlDoc:CreateAttribute( "xmlns" )
            projAttr:Value := "http://schemas.microsoft.com/developer/msbuild/2003"
            xsProj:Attributes:Append( projAttr )
            xmlDoc:AppendChild( xsProj )
            //
            
            VAR import := xmlDoc:CreateElement( "Import" )
            LOCAL configAttr AS XmlAttribute
            configAttr := xmlDoc:CreateAttribute( "Project" )
            configAttr:Value := "$(XSharpMsBuildDir)\XSharp.Default.props"
            import:Attributes:Append( configAttr )
            xsProj:AppendChild( import )
            //
            // Todo Should we generate a relative path ??
            //
            VAR config := xmlDoc:CreateElement("PropertyGroup")
            xsProj:AppendChild( config )
            SELF:AddCompileSettings( config, SELF:Name, stdDef )
            
            VAR references := xmlDoc:CreateElement("ItemGroup")
            xsProj:AppendChild( references )
            FOREACH refer AS Reference IN ReferenceList
                refer:AppendTo( references )
            NEXT
            //
            VAR files := xmlDoc:CreateElement("ItemGroup")
            xsProj:AppendChild( files )
            FOREACH file AS AssetInfo IN FileList
                file:AppendTo( files )
            NEXT
            //
            
            
            config := xmlDoc:CreateElement("PropertyGroup")
            xsProj:AppendChild( config )
            SELF:AddConfigDebug( config )
            
            config := xmlDoc:CreateElement("PropertyGroup")
            xsProj:AppendChild( config )
            SELF:AddConfigRelease( config )
            
            //
            import := xmlDoc:CreateElement( "Import" )
            configAttr := xmlDoc:CreateAttribute( "Project" )
            configAttr:Value := "$(XSharpMsBuildDir)\XSharp.targets"
            import:Attributes:Append( configAttr )
            xsProj:AppendChild( import )
            //
            IF SELF:ProjectReferenceList:Count > 0
                LOCAL dummy AS XmlElement
                VAR libs := xmlDoc:CreateElement("ItemGroup")
                xsProj:AppendChild( libs )
                FOREACH prj AS VSProject IN SELF:ProjectReferenceList
                    import := xmlDoc:CreateElement( "ProjectReference" )
                    configAttr := xmlDoc:CreateAttribute( "Include" )
                    configAttr:Value := prj:Name + ".xsproj"
                    import:Attributes:Append( configAttr )
                    libs:AppendChild( import )
                    //
                    dummy := xmlDoc:CreateElement("Name")
                    dummy:InnerText := prj:Name
                    import:AppendChild( dummy )
                    //
                    dummy := xmlDoc:CreateElement("Project")
                    dummy:InnerText := prj:GUID
                    import:AppendChild( dummy )
                    //
                    dummy := xmlDoc:CreateElement("Private")
                    dummy:InnerText := "True"
                    import:AppendChild( dummy )
                    //
                NEXT
            ENDIF
            //
            xmlDoc:Save( projectFileNameFullPath )
            RETURN
            
            
        PRIVATE METHOD AddConfigCommon(parent AS XmlElement) AS VOID
            VAR xmlDoc := parent:OwnerDocument
            LOCAL dummy AS XmlElement
            dummy := xmlDoc:CreateElement("PlatformTarget")
            dummy:InnerText := "AnyCPU"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("ErrorReport")
            dummy:InnerText := "prompt"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("WarningLevel")
            dummy:InnerText := "4"
            parent:AppendChild( dummy )
            // Remove Warning "Missing RETURN statement"
            // Remove Warning "Ambiguous function RGB()"
            dummy := xmlDoc:CreateElement("DisabledWarnings")
            dummy:InnerText := "XS9025;XS9043"
            parent:AppendChild( dummy )
            
        PRIVATE METHOD AddConfigRelease(parent AS XmlElement ) AS VOID
            VAR xmlDoc := parent:OwnerDocument
            LOCAL configAttr AS XmlAttribute
            configAttr := xmlDoc:CreateAttribute( "Condition" )
            configAttr:Value := "'$(Configuration)|$(Platform)' == 'Release|AnyCPU'"
            parent:Attributes:Append( configAttr )
            configAttr := xmlDoc:CreateAttribute( "Label" )
            configAttr:Value := "Configuration"
            parent:Attributes:Append( configAttr )
            //
            LOCAL dummy AS XmlElement
            dummy := xmlDoc:CreateElement("EmitDebugInformation")
            dummy:InnerText := "False"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("DebugType")
            dummy:InnerText := "pdbonly"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("Optimize")
            dummy:InnerText :="True"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("OutputPath")
            dummy:InnerText := "bin\Release\"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("DefineConstants")
            dummy:InnerText := "VFP2XS;TRACE"
            parent:AppendChild( dummy )
            SELF:AddConfigCommon(parent)
            
            
        PRIVATE METHOD AddConfigDebug(parent AS XmlElement ) AS VOID
            VAR xmlDoc := parent:OwnerDocument
            LOCAL configAttr AS XmlAttribute
            configAttr := xmlDoc:CreateAttribute( "Condition" )
            configAttr:Value := "'$(Configuration)|$(Platform)' == 'Debug|AnyCPU'"
            parent:Attributes:Append( configAttr )
            configAttr := xmlDoc:CreateAttribute( "Label" )
            configAttr:Value := "Configuration"
            parent:Attributes:Append( configAttr )
            //
            LOCAL dummy AS XmlElement
            dummy := xmlDoc:CreateElement("EmitDebugInformation")
            dummy:InnerText := "True"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("DebugType")
            dummy:InnerText := "full"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("Optimize")
            dummy:InnerText :="false"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("OutputPath")
            dummy:InnerText := "bin\Debug\"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("DefineConstants")
            dummy:InnerText := "VFP2XS;DEBUG;TRACE"
            parent:AppendChild( dummy )
            SELF:AddConfigCommon(parent)
            
            
        PRIVATE METHOD AddCompileSettings(parent AS XmlElement, name AS STRING, stdDef AS STRING ) AS VOID
            VAR xmlDoc := parent:OwnerDocument
            LOCAL dummy AS XmlElement
            //dummy := xmlDoc:CreateElement("XSharpProjectExtensionsPath")
            //dummy:InnerText := "$(MSBuildExtensionsPath)\XSharp\"
            //parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("Name")
            dummy:InnerText := name + "App"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("ProjectGuid")
            dummy:InnerText := SELF:GUID
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("OutputType")
            IF SELF:IsLibrary
                dummy:InnerText := "Library"
            ELSE
                dummy:InnerText := "WinExe"
            ENDIF
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("AppDesignerFolder")
            dummy:InnerText :="Properties"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("RootNamespace")
            dummy:InnerText := name
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("AssemblyName")
            IF SELF:IsLibrary
                dummy:InnerText := name + "Lib"
            ELSE
                dummy:InnerText := name + "App"
            ENDIF
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("TargetFrameworkVersion")
            dummy:InnerText := SELF:FrameworkVersion
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("NoLogo")
            dummy:InnerText := "true"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("GenerateFullPaths")
            dummy:InnerText := "true"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("Dialect")
            dummy:InnerText := "FoxPro"
            parent:AppendChild( dummy )
            // ToDo : Late Binding always on ??
            dummy := xmlDoc:CreateElement("LB")
            dummy:InnerText := "true"
            parent:AppendChild( dummy )
            //
            dummy := xmlDoc:CreateElement("OutputName")
            dummy:InnerText := name
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("Undeclared")
            dummy:InnerText := "true"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("MemVar")
            dummy:InnerText := "true"
            parent:AppendChild( dummy )
            //			dummy := xmlDoc:CreateElement("Fox2")
            //			dummy:InnerText := "true"
            //			parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("Fox1")
            dummy:InnerText := "true"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("AllowOldStyleAssignments")
            dummy:InnerText := "true"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("Allowdot")
            dummy:InnerText := "true"
            parent:AppendChild( dummy )
            dummy := xmlDoc:CreateElement("Ins")
            dummy:InnerText := "true"
            parent:AppendChild( dummy )
            IF !String.IsNullOrEmpty( stdDef )
                dummy := xmlDoc:CreateElement("StandardDefs")
                dummy:InnerText := stdDef
                parent:AppendChild( dummy )
            ENDIF
            //			dummy := xmlDoc:CreateElement("StartupObject")
            //			parent:AppendChild( dummy )
            //			dummy := xmlDoc:CreateElement("ApplicationIcon")
            //			parent:AppendChild( dummy )
            //			dummy := xmlDoc:CreateElement("VulcanCompatibleResources")
            //			dummy:InnerText := "False"
            //			parent:AppendChild( dummy )
            //			dummy := xmlDoc:CreateElement("XSharpProjectversion")
            //			dummy:InnerText := "2.1.0.0"
            //			parent:AppendChild( dummy )
            //			dummy := xmlDoc:CreateElement("NoWin32Manifest")
            //			dummy:InnerText := "False"
            //			parent:AppendChild( dummy )
            //			dummy := xmlDoc:CreateElement("NamedArgs")
            //			dummy:InnerText := "False"
            //			parent:AppendChild( dummy )
            
            //
            
            
            
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
