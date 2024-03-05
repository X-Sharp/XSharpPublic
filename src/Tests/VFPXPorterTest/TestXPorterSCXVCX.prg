// TestXPorterSCXVCX.prg
// Created by    : fabri
// Creation Date : 9/27/2023 2:26:50 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING Microsoft.VisualStudio.TestTools.UnitTesting
USING VFPXPorterLib

BEGIN NAMESPACE VFPXPorterTest

    /// <summary>
    /// The TestXPorterSCXVCX class.
    /// </summary>
    [TestClass];
    CLASS TestXPorterSCXVCX

        CONSTRUCTOR()
            RETURN

            [TestMethod];
        METHOD CheckAnalyzeForm() AS VOID  STRICT
            LOCAL xport AS XPorterSCXVCX
            //
            VAR file := "files/members.scx"
            VAR output := "files"
            VAR settings := XPorterSettings{}
            xport := XPorterSCXVCX{}
            xport:Initialize(file, output, settings)
            Assert.AreEqual( TRUE,xport:Analyze())
            VAR log := BufferedSink.Instance:Log
            Console.WriteLine( log )

            [TestMethod];
        METHOD CheckAnalyzeFormBackup() AS VOID  STRICT
            LOCAL xport AS XPorterSCXVCX
            //
            VAR file := "files/members.scx"
            VAR output := "files"
            VAR settings := XPorterSettings{}
            xport := XPorterSCXVCX{}
            xport:Initialize(file, output, settings)
            Assert.AreEqual( TRUE,xport:Analyze( TRUE ))
            VAR log := BufferedSink.Instance:Log
            Console.WriteLine( log )

            [TestMethod];
        METHOD CheckExportFormBackup() AS VOID  STRICT
            LOCAL xport AS XPorterSCXVCX
            //
            VAR file := "files/members.scx"
            VAR output := "files"
            VAR settings := XPorterSettings{}
            xport := XPorterSCXVCX{}
            xport:Initialize(file, output, settings)
            Assert.AreEqual( TRUE,xport:Export( TRUE ))
            VAR log := BufferedSink.Instance:Log
            Console.WriteLine( log )

            [TestMethod];
        METHOD CheckAnalyzeFormSet() AS VOID  STRICT
            LOCAL xport AS XPorterSCXVCX
            //
            VAR file := "files/myformset.scx"
            VAR output := "files"
            VAR settings := XPorterSettings{}
            xport := XPorterSCXVCX{}
            xport:Initialize(file, output, settings)
            Assert.AreEqual( TRUE,xport:Analyze( TRUE ))
            VAR log := BufferedSink.Instance:Log
            Console.WriteLine( log )


            [TestMethod];
        METHOD CheckExportFormSetBackup() AS VOID  STRICT
            LOCAL xport AS XPorterSCXVCX
            //
            VAR file := "files/myformset.scx"
            VAR output := "files"
            VAR settings := XPorterSettings{}
            xport := XPorterSCXVCX{}
            xport:Initialize( file, output, settings)
            Assert.AreEqual( TRUE,xport:Export( TRUE ))
            VAR log := BufferedSink.Instance:Log
            Console.WriteLine( log )


    END CLASS
END NAMESPACE // VFPXPorterTest