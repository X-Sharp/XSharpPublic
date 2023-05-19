using Microsoft.Win32;
using System.ComponentModel;
using XD = LanguageService.CodeAnalysis.XSharp;
using XSharpModel;
namespace XSharp.Project.Options
{
    internal class DebuggerOptions : BaseOptionModel<DebuggerOptions>
    {
        [Category(General)]
        [DisplayName("Allow editing")]
        [Description("Allow editing during debugging")]
        [DefaultValue(false)]
        public bool AllowEditing { get; set; } = false;

        const string General = " General";
        const string DllOptions = "Compiler Options for DLL debugging";
        [Category(DllOptions)]
        [DisplayName(GeneralPropertyPagePanel.captDialect)]
        [Description(GeneralPropertyPagePanel.descDialect)]
        [DefaultValue(XD.XSharpDialect.VO)]
        public XD.XSharpDialect Dialect { get; set; } = XD.XSharpDialect.VO;
        [Category(DllOptions)]
        [DisplayName(LanguagePropertyPagePanel.AZCaption)]
        [Description(LanguagePropertyPagePanel.AZDescription)]
        [DefaultValue(false)]
        public bool ArrayZero { get; set; } = false;

        [Category(DllOptions)]
        [DisplayName(LanguagePropertyPagePanel.MemVarCaption)]
        [Description(LanguagePropertyPagePanel.MemVarDescription)]
        [DefaultValue(false)]
        public bool MemVars { get; set; } = false;

        [Category(DllOptions)]
        [DisplayName(LanguagePropertyPagePanel.UndeclaredCaption)]
        [Description(LanguagePropertyPagePanel.UndeclaredDescription)]
        [DefaultValue(false)]
        public bool UndeclaredMemvars { get; set; } = false;

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO10Caption)]
        [Description(DialectPropertyPagePanel.VO10Description)]
        [DefaultValue(false)]
        public bool Vo10 { get; set; } = false;

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO4Caption)]
        [Description(DialectPropertyPagePanel.VO4Description)]
        [DefaultValue(false)]
        public bool Vo4 { get; set; } = false;

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO12Caption)]
        [Description(DialectPropertyPagePanel.VO12Description)]
        [DefaultValue(false)]
        public bool Vo12 { get; set; } = false;

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO13Caption)]
        [Description(DialectPropertyPagePanel.VO13Description)]
        [DefaultValue(false)]
        public bool Vo13 { get; set; } = false;

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO7Caption)]
        [Description(DialectPropertyPagePanel.VO7Description)]
        [DefaultValue(false)]
        public bool Vo7 { get; set; } = false;

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO6Caption)]
        [Description(DialectPropertyPagePanel.VO6Description)]
        [DefaultValue(false)]
        public bool Vo6 { get; set; } = false;

        [Category(DllOptions)]
        [DisplayName(LanguagePropertyPagePanel.CSCaption)]
        [Description(LanguagePropertyPagePanel.CSDescription)]
        [DefaultValue(false)]
        public bool CaseSensitive { get; set; } = false;
        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO14Caption)]
        [Description(DialectPropertyPagePanel.VO14Description)]
        [DefaultValue(false)]
        public bool Vo14 { get; set; } = false;
        [Category(General)]
        [DisplayName(NoLBCaption)]
        [Description(NoLBDescription)]
        [DefaultValue(false)]
        public bool NoLateBinding { get; set; } = false;
        private const string NoLBCaption = "Disable Late Binding";
        private const string NoLBDescription = "Disable property access and method calls on expressions of type OBJECT and USUAL (/lb-)";
    }
}

