using Microsoft.Win32;
using System.ComponentModel;
using XD = LanguageService.CodeAnalysis.XSharp;
using XSharpModel;
namespace XSharp.Project.Options
{
    internal class DebuggerOptions : BaseOptionModel<DebuggerOptions>
    {
        const string Editor = "Editor";
        [Category(Editor)]
        [DisplayName("Allow editing")]
        [Description("Allow editing during debugging")]
        [DefaultValue(false)]
        public bool AllowEditing { get; set; } = false;

        const string Options = "Options";
        [Category(Options)]
        [DisplayName(GeneralPropertyPagePanel.captDialect)]
        [Description(GeneralPropertyPagePanel.descDialect)]
        [DefaultValue(XD.XSharpDialect.VO)]
        public XD.XSharpDialect Dialect { get; set; } = XD.XSharpDialect.VO;
        [Category(Options)]
        [DisplayName(LanguagePropertyPagePanel.AZCaption)]
        [Description(LanguagePropertyPagePanel.AZDescription)]
        [DefaultValue(false)]
        public bool ArrayZero { get; set; } = false;

        [Category(Options)]
        [DisplayName(LanguagePropertyPagePanel.MemVarCaption)]
        [Description(LanguagePropertyPagePanel.MemVarDescription)]
        [DefaultValue(false)]
        public bool MemVars { get; set; } = false;

        [Category(Options)]
        [DisplayName(LanguagePropertyPagePanel.UndeclaredCaption)]
        [Description(LanguagePropertyPagePanel.UndeclaredDescription)]
        [DefaultValue(false)]
        public bool UndeclaredMemvars { get; set; } = false;

        [Category(Options)]
        [DisplayName(DialectPropertyPagePanel.VO10Caption)]
        [Description(DialectPropertyPagePanel.VO10Description)]
        [DefaultValue(false)]
        public bool Vo10 { get; set; } = false;

        [Category(Options)]
        [DisplayName(DialectPropertyPagePanel.VO4Caption)]
        [Description(DialectPropertyPagePanel.VO4Description)]
        [DefaultValue(false)]
        public bool Vo4 { get; set; } = false;

        [Category(Options)]
        [DisplayName(DialectPropertyPagePanel.VO12Caption)]
        [Description(DialectPropertyPagePanel.VO12Description)]
        [DefaultValue(false)]
        public bool Vo12 { get; set; } = false;

        [Category(Options)]
        [DisplayName(DialectPropertyPagePanel.VO13Caption)]
        [Description(DialectPropertyPagePanel.VO13Description)]
        [DefaultValue(false)]
        public bool Vo13 { get; set; } = false;

        [Category(Options)]
        [DisplayName(DialectPropertyPagePanel.VO7Caption)]
        [Description(DialectPropertyPagePanel.VO7Description)]
        [DefaultValue(false)]
        public bool Vo7 { get; set; } = false;

        [Category(Options)]
        [DisplayName(DialectPropertyPagePanel.VO6Caption)]
        [Description(DialectPropertyPagePanel.VO6Description)]
        [DefaultValue(false)]
        public bool Vo6 { get; set; } = false;

        [Category(Options)]
        [DisplayName(DialectPropertyPagePanel.VO14Caption)]
        [Description(DialectPropertyPagePanel.VO14Description)]
        [DefaultValue(false)]
        public bool Vo14 { get; set; } = false;
    }
}

