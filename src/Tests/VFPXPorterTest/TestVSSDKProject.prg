// TestVSSDKProject.prg
// Tests for the SDK-style .xsproj writer

USING System
USING System.IO
USING System.Linq
USING System.Xml
USING Microsoft.VisualStudio.TestTools.UnitTesting
USING VFPXPorterLib

BEGIN NAMESPACE VFPXPorterTest

    [TestClass];
    CLASS TestVSSDKProject

        PRIVATE folder AS STRING

        [TestInitialize];
        METHOD Setup() AS VOID
            SELF:folder := Path.Combine( Path.GetTempPath(), "VFPXPorterTest_" + Guid.NewGuid():ToString("N") )
            Directory.CreateDirectory( Path.Combine( SELF:folder, "Lib" ) )

        [TestCleanup];
        METHOD Cleanup() AS VOID
            IF Directory.Exists( SELF:folder )
                Directory.Delete( SELF:folder, TRUE )
            ENDIF

        // Save a small main project (2 forms files, 1 data file, 1 library reference) and reload it
        PRIVATE METHOD SaveSample( tfm AS STRING, mode AS XSharpReferenceMode ) AS XmlDocument
            VAR lib := VSSDKProject{ "MyLib", tfm, mode, "3.1.0" }
            lib:ProjectFileNameFullPath := Path.Combine( SELF:folder, "Lib", "MyLib.xsproj" )
            VAR prj := VSSDKProject{ "MyApp", tfm, mode, "3.1.0" }
            prj:AddStandardReferences()
            prj:AddFile( "form1.prg", "Form" )
            prj:AddFile( "form1.designer.prg", "", "form1.prg" )
            prj:AddFile( "Data\customer.dbf", FileAction.CopyAlways )
            prj:ProjectReferenceList:Add( lib )
            VAR projectFile := Path.Combine( SELF:folder, "MyApp.xsproj" )
            VAR header := Path.Combine( SELF:folder, "XSharp", "VFPXPorter.xh" )
            prj:Save( projectFile, prj:FormatStandardDefs( header, SELF:folder ) )
            VAR doc := XmlDocument{}
            doc:Load( projectFile )
            RETURN doc

        PRIVATE STATIC METHOD Value( doc AS XmlDocument, xpath AS STRING ) AS STRING
            VAR node := doc:SelectSingleNode( xpath )
            RETURN IIF( node == NULL, NULL, node:InnerText )

        [TestMethod];
        METHOD WritesSdkProjectWithNuGetPackage() AS VOID
            VAR doc := SELF:SaveSample( "net472", XSharpReferenceMode.NuGet )
            Assert.AreEqual( "Microsoft.NET.Sdk", doc:DocumentElement:GetAttribute( "Sdk" ) )
            Assert.AreEqual( "net472", Value( doc, "/Project/PropertyGroup/TargetFramework" ) )
            Assert.AreEqual( "WinExe", Value( doc, "/Project/PropertyGroup/OutputType" ) )
            Assert.AreEqual( "MyAppApp", Value( doc, "/Project/PropertyGroup/AssemblyName" ) )
            Assert.AreEqual( "true", Value( doc, "/Project/PropertyGroup/UseWindowsForms" ) )
            Assert.AreEqual( "false", Value( doc, "/Project/PropertyGroup/EnableDefaultItems" ) )
            Assert.AreEqual( "$(MSBuildProjectDirectory)\XSharp\VFPXPorter.xh", Value( doc, "/Project/PropertyGroup/StandardDefs" ) )
            // One package; RT and Core come through its dependencies
            VAR packages := doc:SelectNodes( "/Project/ItemGroup/PackageReference" )
            Assert.AreEqual( 1, packages:Count )
            Assert.AreEqual( "XSharp.VFP", ((XmlElement) packages[0]):GetAttribute( "Include" ) )
            Assert.AreEqual( "3.1.0", ((XmlElement) packages[0]):GetAttribute( "Version" ) )
            // Framework assemblies are implicit, except DataSetExtensions on .NET Framework
            VAR refs := doc:SelectNodes( "/Project/ItemGroup/Reference" )
            Assert.AreEqual( 1, refs:Count )
            Assert.AreEqual( "System.Data.DataSetExtensions", ((XmlElement) refs[0]):GetAttribute( "Include" ) )

        [TestMethod];
        METHOD WritesItemsAndProjectReference() AS VOID
            VAR doc := SELF:SaveSample( "net472", XSharpReferenceMode.NuGet )
            Assert.AreEqual( "Form", Value( doc, "/Project/ItemGroup/Compile[@Include='form1.prg']/SubType" ) )
            Assert.AreEqual( "form1.prg", Value( doc, "/Project/ItemGroup/Compile[@Include='form1.designer.prg']/DependentUpon" ) )
            Assert.AreEqual( "Always", Value( doc, "/Project/ItemGroup/None[@Include='Data\customer.dbf']/CopyToOutputDirectory" ) )
            VAR projRef := (XmlElement) doc:SelectSingleNode( "/Project/ItemGroup/ProjectReference" )
            Assert.AreEqual( "Lib\MyLib.xsproj", projRef:GetAttribute( "Include" ) )
            Assert.IsFalse( projRef:HasChildNodes, "SDK ProjectReference needs no Name/Project/Private" )

        [TestMethod];
        METHOD RemovesItemsAddedByXSharpSdkProps() AS VOID
            VAR doc := SELF:SaveSample( "net472", XSharpReferenceMode.NuGet )
            // The cleanup group must come before the explicit Compile items
            VAR groups := doc:SelectNodes( "/Project/ItemGroup" ):Cast<XmlElement>():ToList()
            VAR cleanup := groups:FindIndex( { g => g:SelectSingleNode( "Compile[@Remove='@(Compile)']" ) != NULL } )
            VAR files := groups:FindIndex( { g => g:SelectSingleNode( "Compile[@Include]" ) != NULL } )
            Assert.IsTrue( cleanup >= 0, "missing Compile Remove" )
            Assert.IsTrue( cleanup < files, "cleanup must precede the explicit items" )
            FOREACH VAR itemType IN VSSDKProject.SdkGlobbedItemTypes
                Assert.IsNotNull( groups[cleanup]:SelectSingleNode( itemType + "[@Remove='@(" + itemType + ")']" ), itemType )
            NEXT

        [TestMethod];
        METHOD CompilerSwitchesWrittenOnce() AS VOID
            VAR doc := SELF:SaveSample( "net472", XSharpReferenceMode.NuGet )
            Assert.AreEqual( 1, doc:SelectNodes( "/Project/PropertyGroup/AllowOldStyleAssignments" ):Count )
            Assert.AreEqual( 0, doc:SelectNodes( "/Project/PropertyGroup/OutputName" ):Count )
            Assert.AreEqual( 0, doc:SelectNodes( "/Project/PropertyGroup/ProjectGuid" ):Count )
            Assert.AreEqual( "FoxPro", Value( doc, "/Project/PropertyGroup/Dialect" ) )

        [TestMethod];
        METHOD InstalledAssembliesOnNetFramework() AS VOID
            VAR doc := SELF:SaveSample( "net48", XSharpReferenceMode.InstalledAssemblies )
            Assert.AreEqual( 0, doc:SelectNodes( "/Project/ItemGroup/PackageReference" ):Count )
            VAR hints := doc:SelectNodes( "/Project/ItemGroup/Reference/HintPath" )
            Assert.AreEqual( 6, hints:Count )
            Assert.IsTrue( hints[0]:InnerText:EndsWith( "\Assemblies\XSharp.Core.dll" ) )

        [TestMethod];
        METHOD InstalledAssembliesFallsBackToNuGetOnNet8() AS VOID
            VAR doc := SELF:SaveSample( "net8.0-windows", XSharpReferenceMode.InstalledAssemblies )
            Assert.AreEqual( 1, doc:SelectNodes( "/Project/ItemGroup/PackageReference" ):Count )
            Assert.AreEqual( 0, doc:SelectNodes( "/Project/ItemGroup/Reference" ):Count )

        [TestMethod];
        METHOD DetectsNetFrameworkTargets() AS VOID
            FOREACH VAR tfm IN <STRING>{ "net472", "net48", "net481", "NET40" }
                Assert.IsTrue( VSSDKProject.IsNetFrameworkTarget( tfm ), tfm )
            NEXT
            FOREACH VAR tfm IN <STRING>{ "net8.0-windows", "net8.0", "netstandard2.0", "netcoreapp3.1", "net", "", NULL }
                Assert.IsFalse( VSSDKProject.IsNetFrameworkTarget( tfm ), tfm )
            NEXT

        [TestMethod];
        METHOD EmptyVersionBecomesFloating() AS VOID
            Assert.AreEqual( "*", VSSDKProject{ "X", "net472", XSharpReferenceMode.NuGet, "" }:XSharpPackageVersion )
            Assert.AreEqual( "net472", VSSDKProject{ "X", "", XSharpReferenceMode.NuGet, "1" }:TargetFramework )

    END CLASS

END NAMESPACE
