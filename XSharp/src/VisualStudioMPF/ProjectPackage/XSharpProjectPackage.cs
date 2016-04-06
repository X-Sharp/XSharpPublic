
using System;
using System.Runtime.InteropServices;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Project;
using XSharp.LanguageService;
using XSharp.Project.WPF;
using System.ComponentModel;

namespace XSharp.Project
{
    /// <summary>
    /// This class implements the package exposed by this assembly.
    /// </summary>
    /// <remarks>
    /// <para>A Visual Studio component can be registered under different registry roots; for instance
    /// when you debug your package you want to register it in the experimental hive. This
    /// attribute specifies the registry root to use if no one is provided to regpkg.exe with
    /// the /root switch.</para>
    /// <para>A description of the different attributes used here is given below:</para>
    /// <para>DefaultRegistryRoot: This defines the default registry root for registering the package. 
    /// We are currently using the experimental hive.</para>
    /// <para>ProvideObject: Declares that a package provides creatable objects of specified type.</para> 
    /// <para>ProvideProjectFactory: Declares that a package provides a project factory.</para>
    /// <para>ProvideProjectItem: Declares that a package provides a project item.</para> 
    /// </remarks>  
    [InstalledProductRegistration("#110", "#112", XSharp.Constants.Version, IconResourceID = 400)]
    [Description("XSharp Project System")]
    [PackageRegistration(UseManagedResourcesOnly = true)]
    [DefaultRegistryRoot("Software\\Microsoft\\VisualStudio\\14.0")]
    [ProvideObject(typeof(XSharpGeneralPropertyPage))]      // 53651BEA-799A-45EB-B58C-C884F5417219
    [ProvideObject(typeof(XSharpLanguagePropertyPage))]     // 2652FCA6-1C45-4D25-942D-4C5D5EDE9539
    [ProvideObject(typeof(XSharpBuildPropertyPage))]        // E994C210-9D6D-4CF4-A061-EBBEA2BC626B
    [ProvideObject(typeof(XSharpDebugPropertyPage))]        // 2955A638-C389-4675-BB1C-6B2BC173C1E7
    [ProvideProjectFactory(typeof(XSharpProjectFactory), 
        XSharpConstants.LanguageName, 
        XSharpConstants.LanguageName + " Project Files (*." + XSharpConstants.ProjectExtension + ");*." + XSharpConstants.ProjectExtension,
        XSharpConstants.ProjectExtension,
        XSharpConstants.ProjectExtensions,
        @".NullPath", LanguageVsTemplate = "XSharp", NewProjectRequireNewFolderVsTemplate = false)]


    [ProvideProjectFactory(typeof(XSharpWPFProjectFactory),
        null,
        null,
        null,
        null,
        null,
        LanguageVsTemplate = XSharpConstants.LanguageName,
        TemplateGroupIDsVsTemplate = "WPF",
        ShowOnlySpecifiedTemplatesVsTemplate = false, SortPriority =100)]

    [ProvideProjectItem(typeof(XSharpProjectFactory), "XSharp Items", @"..\..\Templates\ProjectItems\Class", 500)]
    [ProvideProjectItem(typeof(XSharpProjectFactory), "XSharp Items", @"..\..\Templates\ProjectItems\Form", 500)]

    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".prg", Int32.MaxValue,DefaultName = "XSharp Source Code Editor",NameResourceID =109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".xs", Int32.MaxValue, DefaultName = "XSharp Source Code Editor", NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".vh", Int32.MaxValue, DefaultName = "XSharp Source Code Editor", NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".xh", Int32.MaxValue, DefaultName = "XSharp Source Code Editor", NameResourceID = 109)]
    [ProvideEditorExtension(typeof(XSharpEditorFactory), ".ppo", Int32.MaxValue, DefaultName = "XSharp Source Code Editor", NameResourceID = 109)]
    // Todo
    // Add extensions for VO Compatible editors here


    // Attention! The LOGVIEWID guids are magic numbers provided by Microsoft. Don't change them.
    [ProvideEditorLogicalView(typeof(XSharpEditorFactory), "{7651a702-06e5-11d1-8ebd-00a0c90f26ea}")]  //LOGVIEWID_Designer
    [ProvideEditorLogicalView(typeof(XSharpEditorFactory), "{7651a701-06e5-11d1-8ebd-00a0c90f26ea}")]  //LOGVIEWID_Code

    [Guid(GuidStrings.guidXSharpProjectPkgString)]
    public sealed class XSharpProjectPackage : ProjectPackage
    {
        #region Overridden Implementation
        /// <summary>
        /// Initialization of the package; this method is called right after the package is sited, so this is the place
        /// where you can put all the initialization code that rely on services provided by VisualStudio.
        /// </summary>
        protected override void Initialize()
        {
            base.Initialize();
            this.RegisterProjectFactory(new XSharpProjectFactory(this));

            // Indicate how to open the different source files : SourceCode or Designer ??
            this.RegisterEditorFactory(new XSharpEditorFactory(this));

            this.RegisterProjectFactory(new XSharpWPFProjectFactory(this));

            // Load the language service
            XSharpLanguageService service = GetService(typeof(XSharpLanguageService)) as XSharpLanguageService;
        }

        public override string ProductUserContext
        {
            get { return "XSharp"; }
        }

        #endregion
    }
}
