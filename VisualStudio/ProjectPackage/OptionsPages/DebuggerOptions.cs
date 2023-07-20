using System.ComponentModel;
using XSharpModel;
using XD = LanguageService.CodeAnalysis.XSharp;
using XSharp.Settings;
namespace XSharp.Project.Options
{
    public class DebuggerOptions : BaseOptionModel<DebuggerOptions>
    {
        [Category(General)]
        [DisplayName("Allow editing")]
        [Description("Allow editing during debugging")]
        [DefaultValue(false)]
        public bool AllowEditing { get; set; }

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
        public bool Vo4 { get; set; } 

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO12Caption)]
        [Description(DialectPropertyPagePanel.VO12Description)]
        [DefaultValue(false)]
        public bool Vo12 { get; set; }

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO13Caption)]
        [Description(DialectPropertyPagePanel.VO13Description)]
        [DefaultValue(false)]
        public bool Vo13 { get; set; } 

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO7Caption)]
        [Description(DialectPropertyPagePanel.VO7Description)]
        [DefaultValue(false)]
        public bool Vo7 { get; set; } 

        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO6Caption)]
        [Description(DialectPropertyPagePanel.VO6Description)]
        [DefaultValue(false)]
        public bool Vo6 { get; set; } 

        [Category(DllOptions)]
        [DisplayName(LanguagePropertyPagePanel.CSCaption)]
        [Description(LanguagePropertyPagePanel.CSDescription)]
        [DefaultValue(false)]
        public bool CaseSensitive { get; set; } 
        [Category(DllOptions)]
        [DisplayName(DialectPropertyPagePanel.VO14Caption)]
        [Description(DialectPropertyPagePanel.VO14Description)]
        [DefaultValue(false)]
        public bool Vo14 { get; set; } 
        [Category(General)]
        [DisplayName(NoLBCaption)]
        [Description(NoLBDescription)]
        [DefaultValue(false)]
        public bool NoLateBinding { get; set; } 
        private const string NoLBCaption = "Disable Late Binding";
        private const string NoLBDescription = "Disable property access and method calls on expressions of type OBJECT and USUAL (/lb-)";


        public override void WriteToSettings()
        {
            XDebuggerSettings.ArrayZero = this.ArrayZero;
            XDebuggerSettings.AllowEditing = this.AllowEditing;
            XDebuggerSettings.Dialect = (int)this.Dialect;
            XDebuggerSettings.MemVars = this.MemVars;
            XDebuggerSettings.UndeclaredMemvars = this.UndeclaredMemvars;
            XDebuggerSettings.Vo4 = this.Vo4;
            XDebuggerSettings.Vo6 = this.Vo6;
            XDebuggerSettings.Vo7 = this.Vo7;
            XDebuggerSettings.Vo10 = this.Vo10;
            XDebuggerSettings.Vo12 = this.Vo12;
            XDebuggerSettings.Vo13 = this.Vo13;
            XDebuggerSettings.Vo14 = this.Vo14;
            XDebuggerSettings.NoLateBinding = this.NoLateBinding;
            XDebuggerSettings.CaseSensitive = this.CaseSensitive;
        }
    }
}

