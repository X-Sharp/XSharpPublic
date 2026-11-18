// TestTemplateHelper.prg
// Tests for TemplateHelper.ReplaceAndValidate placeholder substitution

USING System
USING System.Collections.Generic
USING Microsoft.VisualStudio.TestTools.UnitTesting
USING VFPXPorterLib

BEGIN NAMESPACE VFPXPorterTest

    [TestClass];
    CLASS TestTemplateHelper

        [TestMethod];
        METHOD ReplacesKnownPlaceholders() AS VOID
            VAR repl := Dictionary<STRING, STRING>{}
            repl["formName"] := "Form1"
            repl["superName"] := "XSharp.VFP.UI.Form"
            VAR result := TemplateHelper.ReplaceAndValidate("CLASS <@formName@> INHERIT <@superName@>", "T", repl)
            Assert.AreEqual("CLASS Form1 INHERIT XSharp.VFP.UI.Form", result)

        [TestMethod];
        METHOD PlaceholderCaseIsIgnored() AS VOID
            VAR repl := Dictionary<STRING, STRING>{}
            repl["formName"] := "Form1"
            VAR result := TemplateHelper.ReplaceAndValidate("CLASS <@FORMNAME@>", "T", repl)
            Assert.AreEqual("CLASS Form1", result)

        [TestMethod];
        METHOD UnknownPlaceholderIsEmptiedNotThrown() AS VOID
            VAR repl := Dictionary<STRING, STRING>{}
            repl["formName"] := "Form1"
            VAR result := TemplateHelper.ReplaceAndValidate("A<@frmName@>B <@formName@>", "T", repl)
            Assert.AreEqual("AB Form1", result)

        [TestMethod];
        METHOD ValuesAreNotRescanned() AS VOID
            VAR repl := Dictionary<STRING, STRING>{}
            repl["EventHandlers"] := "x := '<@formName@>' // <@ user code"
            repl["formName"] := "Form1"
            VAR result := TemplateHelper.ReplaceAndValidate("<@EventHandlers@>|<@formName@>", "T", repl)
            Assert.AreEqual("x := '<@formName@>' // <@ user code|Form1", result)

        [TestMethod];
        METHOD NullValueAndUnusedKeys() AS VOID
            VAR repl := Dictionary<STRING, STRING>{}
            repl["DoBindings"] := NULL
            repl["notInTemplate"] := "ignored"
            VAR result := TemplateHelper.ReplaceAndValidate("[<@DoBindings@>]", "T", repl)
            Assert.AreEqual("[]", result)

    END CLASS

END NAMESPACE
