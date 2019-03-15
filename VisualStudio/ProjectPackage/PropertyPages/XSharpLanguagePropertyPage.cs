//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
using System;
using System.IO;
using System.Runtime.InteropServices;
using System.Runtime.Versioning;
using System.Windows.Forms;
using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.Project;
using EnvDTE;
using EnvDTE80;
using System.ComponentModel;
using System.Linq;
using Microsoft.VisualStudio.OLE.Interop;

namespace XSharp.Project
{
    /// <summary>
    /// This class implements language property page for the project type.
    /// </summary>
    [ComVisible(true)]
    [Guid("0DFC7EF7-3F1A-4ACB-AFD8-DF56AEF9467A")]
    [ClassInterface(ClassInterfaceType.AutoDual)]
    public class XSharpLanguagePropertyPage : XSharpSettingsPage
    {
        #region Constants
        internal const string LanguageCaption = "Language";
        internal const string CMDCaption = "Extra Command Line Options";
        internal const string AZCaption = "Use Zero Based Arrays";
        internal const string CSCaption = "Case Sensitive";
        internal const string INSCaption = "Enable Implicit Namespace lookup";
        internal const string LBCaption = "Allow Late Binding";
        internal const string NamedArgCaption = "Allow Named Arguments";
        internal const string NSCaption = "Prefix classes with default Namespace";
        internal const string OVFCaption = "Overflow Exceptions";
        internal const string UnsafeCaption = "Allow Unsafe Code";
        internal const string MemVarCaption = "Enable Memvar support";
        internal const string UndeclaredCaption = "Enable Undeclared variables support";

        internal const string CSDescription = "Enable/Disable case sensitivity (/cs)";
        internal const string AZDescription = "Use Zero Based Arrays (/az)";
        internal const string INSDescription = "Enable the implicit lookup of classes defined in assemblies with an Implicit Namespace attribute (/ins)";
        internal const string LBDescription = "Allow property access and method calls on expressions of type OBJECT and USUAL (/lb)";
        internal const string NamedArgDescription = "Allow named arguments (Default = FALSE for the Core dialect and TRUE for the other dialects). Changing the dialect may also automatically change this setting. (/namedargs)";
        internal const string NSDescription = "Prefix all classes that do not have a namespace prefix and are not in a begin namespace ... end namespace block with the namespace of the assembly (/ns:<Namespace>)";
        internal const string OVFDescription = "Check for Overflow and Underflow for numeric expressions, like the CHECKED keyword. (/ovf)";
        internal const string UnsafeDescription = "Allow Unsafe code inside this assembly (/unsafe)";

        internal const string NoStdDefCaption = "Suppress standard header file";
        internal const string NoStdDefDescription = "Suppress inclusion of the standard header file (XSharpDefs.xh) in every file (/nostddef)";
        internal const string INCCaption = "Additional Include paths";
        internal const string INCDescription = "Additional include paths for the preprocessor (it also looks through the folders set with the include environment variable) (/i)";
        internal const string StdDefCaption = "Alternate standard header file";
        internal const string StdDefDescription = "Name of an alternative standard header file (alternative for XSharpDefs.xh)  (/stddefs)";
        internal const string MemVarDescription = "Enable support for memory variables (MEMVAR, PUBLIC, PRIVATE & PARAMETERS). (/memvar)\rPlease note that this is NOT supported for the Core and Vulcan dialects";
        internal const string UndeclaredDescription = "Enable support for undeclared variables (these are resolved to MEMVARs). (/undeclared)\rPlease note that this requires /memvar to be enabled as well.";

        internal const string CatGeneral = "General";
        internal const string CatNamespaces = "Namespaces";
        internal const string CatPreprocessor = "Preprocessor";
        internal const string CatMemVars = "Memory Variables";

        #endregion
        #region Fields
        private bool saving;
        private bool @unsafe;
        private bool az;
        private bool cs;
        private bool ins;
        private bool lb;
        private bool namedargs;
        private bool ns;
        private bool ovf;
        private string includepaths;
        private bool nostandarddefs;
        private string standarddefs;
        private bool memvar;
        private bool undeclared;

        #endregion Fields

        #region Constructors
        /// <summary>
        /// Explicitly defined default constructor.
        /// </summary>
        public XSharpLanguagePropertyPage()
        {
            this.Name = LanguageCaption;
        }

        #endregion

        #region Properties
        [Category(CatGeneral), DisplayName(UnsafeCaption), Description(UnsafeDescription)]

        public bool Unsafe
        {
            get { return this.@unsafe; }
            set { this.@unsafe = value; this.IsDirty = true; }
        }

        [Category(CatGeneral), DisplayName(AZCaption), Description(AZDescription)]
        public bool AZ
        {
            get { return this.az; }
            set { this.az = value; this.IsDirty = true; }
        }

        [Category(CatGeneral), DisplayName(CSCaption), Description(CSDescription)]
        public bool CS
        {
            get { return this.cs; }
            //set { this.cs = value; this.IsDirty = true; }
        }

        [Category(CatGeneral), DisplayName(OVFCaption), Description(OVFDescription)]
        public bool OVF
        {
            get { return this.ovf; }
            set { this.ovf = value; this.IsDirty = true; }
        }

        [Category(CatGeneral), DisplayName(LBCaption), Description(LBDescription)]
        public bool LB
        {
            get { return this.lb; }
            set { this.lb = value; this.IsDirty = true; }
        }

        [Category(CatGeneral), DisplayName(NamedArgCaption), Description(NamedArgDescription)]
        public bool NamedArgs
        {
            get { return this.namedargs; }
            set { this.namedargs = value; this.IsDirty = true; }
        }
        [Category(CatNamespaces), DisplayName(INSCaption), Description(INSDescription)]
        public bool INS
        {
            get { return this.ins; }
            set { this.ins = value; this.IsDirty = true; }
        }

        [Category(CatNamespaces), DisplayName(NSCaption), Description(NSDescription)]
        public bool NS
        {
            get { return this.ns; }
            set { this.ns = value; this.IsDirty = true; }
        }
        [Category(CatPreprocessor), DisplayName(INCCaption), Description(INCDescription)]
        [Editor(typeof(XSharpSLEPropertyEditor), typeof(System.Drawing.Design.UITypeEditor))]
        public string IncludePaths
        {
            get { return this.includepaths; }
            set { this.includepaths = value; this.IsDirty = true; }
        }

        [Category(CatPreprocessor), DisplayName(NoStdDefCaption), Description(NoStdDefDescription)]
        public bool NoStandardDefs
        {
            get { return this.nostandarddefs; }
            set { this.nostandarddefs = value; this.IsDirty = true; EnableDisableStandardDefs(); }
        }
        [Category(CatMemVars), DisplayName(MemVarCaption), Description(MemVarDescription)]
        public bool MemVar
        {
            get { return this.memvar; }
            set { this.memvar = value; this.IsDirty = true; this.EnableMemVars(); }
        }

        [Category(CatMemVars), DisplayName(UndeclaredCaption), Description(UndeclaredDescription)]
        [ReadOnly(true)]
        public bool Undeclared
        {
            get { return this.undeclared; }
            set { this.undeclared = value; this.IsDirty = true; }
        }

        private void EnableDisableStandardDefs()
        {
            SetFieldReadOnly("StandardDefs", nostandarddefs);
        }

        [Category(CatPreprocessor), DisplayName(StdDefCaption), Description(StdDefDescription)]
        [Editor(typeof(XSharpFileNameEditor), typeof(System.Drawing.Design.UITypeEditor))]
        [XSharpFileNameEditorAttribute("Select Alternative Standard Header File", "Header Files (*.xh; *.vh; *.ch)|*.xh;*.vh;*.ch|All files (*.*)|*.*", 0)]
        [ReadOnly(true)]
        public string StandardDefs
        {
            get { return this.standarddefs; }
            set { this.standarddefs = value; this.IsDirty = true; }
        }


        #endregion
        #region Overriden Implementation
        /// <summary>
        /// Returns class FullName property value.
        /// </summary>
        public override string GetClassName()
        {
            return this.GetType().FullName;
        }

        internal override void Project_OnProjectPropertyChanged(object sender, ProjectPropertyChangedArgs e)
        {
            if (!saving)
            {

                if (e.PropertyName.ToLower() == nameof(NamedArgs).ToLower())
                {
                    BindNamedArgs();
                    Grid.Refresh();
                }
                if (e.PropertyName.ToLower() == nameof(MemVar).ToLower() ||
                    e.PropertyName.ToLower() == nameof(Undeclared).ToLower())
                {
                    ReadMemvars();
                    Grid.Refresh();
                }
            }
        }


        private void BindNamedArgs()
        {
            string tmp = getPrjString(nameof(NamedArgs));
            if (string.IsNullOrEmpty(tmp))
            {
                tmp = getPrjString("Dialect");
                namedargs = string.Compare(tmp, "Core", true) == 0;
            }
            else
            {
                namedargs = getPrjLogic(nameof(NamedArgs), false);
            }

        }

        private void ReadMemvars()
        {
            memvar = getPrjLogic(nameof(MemVar), false);
            undeclared = getPrjLogic(nameof(Undeclared), false);
        }
        private void EnableMemVars()
        {
            if (memvar == false)
            {
                undeclared = false;
                SetFieldReadOnly(nameof(Undeclared), true);
            }
            else
            {
                SetFieldReadOnly(nameof(Undeclared), false);
            }

        }

        /// <summary>
        /// Bind properties.
        /// </summary>
        ///


        protected override void BindProperties()
        {
            if (this.ProjectMgr == null)
            {
                return;
            }
            az = getPrjLogic(nameof(AZ), false);
            cs = getPrjLogic(nameof(CS), false);
            lb = getPrjLogic(nameof(LB), false);
            ovf = getPrjLogic(nameof(OVF), false);
            @unsafe = getPrjLogic(nameof(Unsafe), false);

            ins = getPrjLogic(nameof(INS), false);
            ns = getPrjLogic(nameof(NS), false);

            nostandarddefs = getPrjLogic(nameof(NoStandardDefs), false);
            includepaths = getPrjString(nameof(IncludePaths), "",true);
            standarddefs = getPrjString(nameof(StandardDefs), "",true);

            BindNamedArgs();
            ReadMemvars();
            EnableDisableStandardDefs();
            EnableMemVars();
        }

        /// <summary>
        /// Apply Changes on project node.
        /// </summary>
        /// <returns>E_INVALIDARG if internal ProjectMgr is null, otherwise applies changes and return S_OK.</returns>
        protected override int ApplyChanges()
        {
            if (this.ProjectMgr == null)
            {
                return VSConstants.E_INVALIDARG;
            }
            saving = true;

            this.ProjectMgr.SetProjectProperty(nameof(AZ), this.az.ToString().ToLower());

            this.ProjectMgr.SetProjectProperty(nameof(CS), this.cs.ToString().ToLower());
            this.ProjectMgr.SetProjectProperty(nameof(LB), this.lb.ToString().ToLower());
            this.ProjectMgr.SetProjectProperty(nameof(NamedArgs), this.namedargs.ToString().ToLower());
            this.ProjectMgr.SetProjectProperty(nameof(OVF), this.ovf.ToString().ToLower());
            this.ProjectMgr.SetProjectProperty(nameof(Unsafe), this.@unsafe.ToString().ToLower());


            this.ProjectMgr.SetProjectProperty(nameof(INS), this.ins.ToString().ToLower());
            this.ProjectMgr.SetProjectProperty(nameof(NS), this.ns.ToString().ToLower());
            ((XSharpProjectNode)this.ProjectMgr).PrefixClassesWithDefaultNamespace = this.ns;

            this.ProjectMgr.SetProjectProperty(nameof(NoStandardDefs), this.nostandarddefs.ToString().ToLower());
            this.ProjectMgr.SetProjectProperty(nameof(IncludePaths), this.includepaths?.ToString());
            this.ProjectMgr.SetProjectProperty(nameof(StandardDefs), this.standarddefs?.ToString());
            this.ProjectMgr.SetProjectProperty(nameof(MemVar), this.memvar.ToString().ToLower());
            this.ProjectMgr.SetProjectProperty(nameof(Undeclared), this.undeclared.ToString().ToLower());
            saving = false;
            this.IsDirty = false;

            return VSConstants.S_OK;
        }
        #endregion
    }

}
