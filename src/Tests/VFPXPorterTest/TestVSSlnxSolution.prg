// TestVSSlnxSolution.prg
// Tests for the .slnx solution writer/reader

USING System
USING System.IO
USING System.Xml
USING Microsoft.VisualStudio.TestTools.UnitTesting
USING VFPXPorterLib

BEGIN NAMESPACE VFPXPorterTest

    [TestClass];
    CLASS TestVSSlnxSolution

        PRIVATE folder AS STRING

        [TestInitialize];
        METHOD Setup() AS VOID
            SELF:folder := Path.Combine( Path.GetTempPath(), "VFPXPorterTest_" + Guid.NewGuid():ToString("N") )
            Directory.CreateDirectory( SELF:folder )

        [TestCleanup];
        METHOD Cleanup() AS VOID
            IF Directory.Exists( SELF:folder )
                Directory.Delete( SELF:folder, TRUE )
            ENDIF

        PRIVATE METHOD Reload( slnFile AS STRING ) AS XmlDocument
            VAR doc := XmlDocument{}
            doc:Load( slnFile )
            RETURN doc

        [TestMethod];
        METHOD WritesProjectsWithXSharpType() AS VOID
            VAR sln := VSSlnxSolution{}
            Assert.AreEqual( ".slnx", sln:FileExtension )
            VAR main := VSProject{ "MyApp" }
            main:RelativePath := "MyApp.xsproj"
            VAR lib := VSProject{ "MyLib" }
            lib:RelativePath := "Libs\MyLib\MyLib.xsproj"
            sln:Projects:Add( main )
            sln:Projects:Add( lib )
            VAR slnFile := Path.Combine( SELF:folder, "MyApp.slnx" )
            sln:Save( slnFile )
            Assert.IsFalse( File.ReadAllText( slnFile ):StartsWith( "<?xml" ), "no XML declaration, like VS" )
            VAR doc := SELF:Reload( slnFile )
            Assert.AreEqual( "Solution", doc:DocumentElement:Name )
            VAR projects := doc:SelectNodes( "/Solution/Project" )
            Assert.AreEqual( 2, projects:Count )
            Assert.AreEqual( "MyApp.xsproj", ((XmlElement) projects[0]):GetAttribute( "Path" ) )
            Assert.AreEqual( "Libs/MyLib/MyLib.xsproj", ((XmlElement) projects[1]):GetAttribute( "Path" ) )
            Assert.AreEqual( VSSlnxSolution.XSharpProjectType, ((XmlElement) projects[1]):GetAttribute( "Type" ) )

        [TestMethod];
        METHOD WritesSdkProjectsWithSdkType() AS VOID
            VAR sln := VSSlnxSolution{}
            VAR project := VSSDKProject{ "SdkApp", "net472", XSharpReferenceMode.NuGet, "1.0.0" }
            project:RelativePath := "SdkApp.xsproj"
            sln:Projects:Add( project )
            VAR slnFile := Path.Combine( SELF:folder, "SdkApp.slnx" )
            sln:Save( slnFile )
            VAR doc := SELF:Reload( slnFile )
            VAR sdkProject := (XmlElement) doc:SelectSingleNode( "/Solution/Project" )
            Assert.AreEqual( VSSlnxSolution.SdkXSharpProjectType, sdkProject:GetAttribute( "Type" ) )

        [TestMethod];
        METHOD AppendKeepsExistingTypesAndFolders() AS VOID
            VAR slnFile := Path.Combine( SELF:folder, "Existing.slnx" )
            File.WriteAllText( slnFile, ;
                "<Solution>" + ;
                "  <Configurations><ProjectType Name=""XSharp"" TypeId=""" + VSSlnxSolution.XSharpProjectType + """ Extension="".xsproj"" /></Configurations>" + ;
                "  <Folder Name=""/Tools/"">" + ;
                "    <File Path=""Tools/readme.txt"" />" + ;
                "    <Folder Name=""/Tools/Nested/""><File Path=""Tools/Nested/item.txt"" /></Folder>" + ;
                "    <Project Path=""Tools/Helper/Helper.csproj"" />" + ;
                "  </Folder>" + ;
                "  <Project Path=""Old/Old.xsproj"" Type=""" + VSSlnxSolution.XSharpProjectType + """ />" + ;
                "</Solution>" )
            VAR sln := VSSlnxSolution{}
            Assert.IsTrue( sln:Load( slnFile ) )
            Assert.AreEqual( 2, sln:Projects:Count )
            Assert.AreEqual( "Helper", sln:Projects[0]:Name )
            VAR added := VSProject{ "MyApp" }
            added:RelativePath := "MyApp\MyApp.xsproj"
            sln:Projects:Add( added )
            sln:Save( slnFile )
            VAR doc := SELF:Reload( slnFile )
            // The C# project stays in its folder and gets no X# Type
            VAR helper := (XmlElement) doc:SelectSingleNode( "/Solution/Folder[@Name='/Tools/']/Project" )
            Assert.IsNotNull( helper )
            Assert.AreEqual( "Tools/Helper/Helper.csproj", helper:GetAttribute( "Path" ) )
            Assert.IsFalse( helper:HasAttribute( "Type" ) )
            Assert.IsNotNull( doc:SelectSingleNode( "/Solution/Configurations/ProjectType[@Name='XSharp']" ) )
            Assert.IsNotNull( doc:SelectSingleNode( "/Solution/Folder[@Name='/Tools/']/File[@Path='Tools/readme.txt']" ) )
            Assert.IsNotNull( doc:SelectSingleNode( "/Solution/Folder[@Name='/Tools/']/Folder[@Name='/Tools/Nested/']/File[@Path='Tools/Nested/item.txt']" ) )
            // The new project is at the root, with the X# Type
            VAR mine := (XmlElement) doc:SelectSingleNode( "/Solution/Project[@Path='MyApp/MyApp.xsproj']" )
            Assert.IsNotNull( mine )
            Assert.AreEqual( VSSlnxSolution.XSharpProjectType, mine:GetAttribute( "Type" ) )
            Assert.AreEqual( 3, doc:SelectNodes( "//Project" ):Count )

        [TestMethod];
        METHOD AppendReplacementUsesNewProjectType() AS VOID
            VAR slnFile := Path.Combine( SELF:folder, "Existing.slnx" )
            File.WriteAllText( slnFile, ;
                "<Solution><Project Path=""MyApp.xsproj"" Type=""" + VSSlnxSolution.SdkXSharpProjectType + """ /></Solution>" )
            VAR sln := VSSlnxSolution{}
            Assert.IsTrue( sln:Load( slnFile ) )
            sln:Projects:Remove( sln:Projects[0] )
            VAR replacement := VSProject{ "MyApp" }
            replacement:RelativePath := "MyApp.xsproj"
            sln:Projects:Add( replacement )
            sln:Save( slnFile )
            VAR doc := SELF:Reload( slnFile )
            VAR project := (XmlElement) doc:SelectSingleNode( "/Solution/Project" )
            Assert.AreEqual( VSSlnxSolution.XSharpProjectType, project:GetAttribute( "Type" ) )

        [TestMethod];
        METHOD LoadMissingOrInvalidFile() AS VOID
            VAR sln := VSSlnxSolution{}
            Assert.IsFalse( sln:Load( Path.Combine( SELF:folder, "none.slnx" ) ) )
            VAR bad := Path.Combine( SELF:folder, "bad.slnx" )
            File.WriteAllText( bad, "not xml" )
            Assert.IsFalse( sln:Load( bad ) )
            Assert.AreEqual( 0, sln:Projects:Count )

    END CLASS

END NAMESPACE
