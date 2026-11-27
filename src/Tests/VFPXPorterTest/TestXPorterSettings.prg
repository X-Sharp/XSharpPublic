// TestXPorterSettings.prg
// Tests for the project-format settings of XPorterSettings

USING System
USING System.IO
USING Microsoft.VisualStudio.TestTools.UnitTesting
USING VFPXPorterLib

BEGIN NAMESPACE VFPXPorterTest

    [TestClass];
    CLASS TestXPorterSettings

        [TestMethod];
        METHOD ProjectFormatDefaultsKeepClassicOutput() AS VOID
            VAR settings := XPorterSettings{}
            Assert.AreEqual( ProjectFormat.Classic, settings:ProjectFormat )
            Assert.AreEqual( "net472", settings:TargetFramework )
            Assert.AreEqual( "4.7.2", settings:FrameworkVersion )
            Assert.AreEqual( XSharpReferenceMode.NuGet, settings:XSharpReferences )
            Assert.AreEqual( "", settings:XSharpPackageVersion )
            Assert.AreEqual( SolutionFormat.Sln, settings:SolutionFormat )

        [TestMethod];
        METHOD ExplicitPackageVersionWins() AS VOID
            VAR settings := XPorterSettings{}
            settings:XSharpPackageVersion := " 3.0.0.4 "
            Assert.AreEqual( "3.0.0.4", settings:ResolveXSharpPackageVersion() )

        [TestMethod];
        METHOD ResolvedPackageVersionIsNeverEmpty() AS VOID
            // Depends on the machine: the installed X# version, or "*" when X# is not installed
            VAR version := XPorterSettings{}:ResolveXSharpPackageVersion()
            Assert.IsFalse( String.IsNullOrEmpty( version ) )

        [TestMethod];
        METHOD FindPackageVersionTakesHighest() AS VOID
            VAR folder := Path.Combine( Path.GetTempPath(), "VFPXPorterTest_" + Guid.NewGuid():ToString("N") )
            Directory.CreateDirectory( folder )
            TRY
                FOREACH VAR name IN <STRING>{ "XSharp.VFP.3.0.0.4.nupkg", "XSharp.VFP.3.9.0.nupkg", "XSharp.VFP.3.10.0.nupkg", ;
                                               "XSharp.VFP.4.0.0-beta.nupkg", "XSharp.Core.9.0.0.nupkg" }
                    File.WriteAllText( Path.Combine( folder, name ), "" )
                NEXT
                // Numeric comparison (3.10 > 3.9); pre-release and other packages are ignored
                Assert.AreEqual( "3.10.0", XPorterSettings.FindXSharpPackageVersion( folder ) )
            FINALLY
                Directory.Delete( folder, TRUE )
            END TRY

        [TestMethod];
        METHOD FindPackageVersionMissingFolder() AS VOID
            Assert.AreEqual( "", XPorterSettings.FindXSharpPackageVersion( "Z:\\NoSuchFolder\\Nuget" ) )
            Assert.AreEqual( "", XPorterSettings.FindXSharpPackageVersion( "" ) )

    END CLASS

END NAMESPACE
