using System;
using System.Collections;
using System.Collections.Generic;
using System.Reflection;
using System.Diagnostics;
using System.IO;
using System.Collections.Immutable;
namespace XSharpModel
{
    [DebuggerDisplay("{DisplayName,nq}")]
    public class AssemblyInfo
    {
        // Fields
        /// <summary>
        /// A Dictionary of Type : The key is a string with the typeName in LowerCase
        /// </summary>
        private IDictionary<string, Type> _aTypes;
        private IList<MethodInfo> _aExtensions;

        private string _FileName;
        private DateTime _Modified;
        private bool lLoadedTypes;
        private Assembly _Assembly;
        private string _GlobalClassName;

        private Hashtable _NameSpaces;
        private List<string> _NameSpaceTexts;
        private List<string> _ImplicitNamespaces;
        private NameSpaceContainer _ZeroNamespace;
        private VSLangProj.Reference _Reference;

        // Has Extensions Methods ?
        private bool lHasExtensions;
        
        public string DisplayName
        {
            get
            {
                if (string.IsNullOrEmpty(_FileName))
                    return "(Empty)";
                return System.IO.Path.GetFileName(_FileName);
            }
        }
        public string FileName
        {
            get
            {
                return _FileName;
            }

            internal set
            {
                _FileName = value;
            }
        }

        public DateTime Modified
        {
            get
            {
                return _Modified;
            }

            internal set
            {
                _Modified = value;
            }
        }

        public Assembly Assembly
        {
            get
            {
                return _Assembly;
            }

            internal set
            {
                _Assembly = value;
            }
        }

        public IDictionary<string, Type> Types
        {
            get
            {
                return _aTypes;
            }
        }
        public ImmutableList<string> ImplicitNamespaces => _ImplicitNamespaces.ToImmutableList();
        public String GlobalClassName => _GlobalClassName;

        public ImmutableList<string> Namespaces => _NameSpaceTexts.ToImmutableList();


        public AssemblyInfo()
        {
            // A
            this._aTypes = new Dictionary<string, Type>(StringComparer.OrdinalIgnoreCase);
            this._aExtensions = new List<MethodInfo>();
            this._NameSpaces = new Hashtable(StringComparer.OrdinalIgnoreCase);
            this._NameSpaceTexts = new List<string>();
            this._ImplicitNamespaces = new List<string>();
            this._ZeroNamespace = new NameSpaceContainer("_");
            this._Assembly = null;

        }
        public AssemblyInfo (VSLangProj.Reference reference) : this()
        {
            _Reference = reference;
        }
        public AssemblyInfo(string _cFileName, DateTime _dModified) : this()
        {
            this.FileName = _cFileName;
            this.Modified = _dModified;

            this.UpdateAssembly();
        }


        public static Assembly LoadAssemblyFromFile(string FileName)
        {
            if (!System.IO.File.Exists(FileName))
            {
                return null;
            }
            try
            {
                FileStream input = new FileStream(FileName, FileMode.Open, FileAccess.Read);
                byte[] rawAssembly = new BinaryReader(input).ReadBytes((int)input.Length);
                if (rawAssembly.Length != input.Length)
                {
                    //MessageBox.Show("Intellisense error 9");
                }
                input.Close();

                // if the PDB file exists then this might put a lock on the pdb file.
                // so we rename the pdb temporarily to prevent the lock
                var cPdb = Path.ChangeExtension(FileName, ".pdb");
                var cTmp = Path.ChangeExtension(FileName, ".p$$");
                bool renamed = false;
                if (File.Exists(cPdb))
                {
                    renamed = true;
                    if (File.Exists(cTmp))
                        File.Delete(cTmp);
                    File.Move(cPdb, cTmp);
                }
                var assembly = Assembly.Load(rawAssembly);
                if (renamed && File.Exists(cTmp))
                {
                    File.Move(cTmp, cPdb);
                }
                input.Dispose();
                return assembly;
            }
            catch
            {
            }
            return null;

        }

        internal void LoadAssembly()
        {
            if (String.IsNullOrEmpty(FileName))
            {
                if (_Reference != null)
                {
                    FileName = _Reference.Path;
                }
            }
            if (!System.IO.File.Exists(FileName))
            {
                return ;
            }
            this.Assembly = LoadAssemblyFromFile(FileName);
            this.Modified = File.GetLastWriteTime(FileName);
        }

        internal void UpdateAssembly()
        {
            Type[] types = null;
            var aTypes = new Dictionary<string, Type>(StringComparer.OrdinalIgnoreCase);
            var aExtensions = new List<MethodInfo>();
            //
            int num;
            string nspace = "";
            string fullName = "";
            string simpleName = "";
            //
            this._NameSpaces.Clear();
            this._NameSpaceTexts.Clear();
            
            this._ZeroNamespace.Types.Clear();
            this._GlobalClassName = "";
            this.lHasExtensions = false;
            //
            this.LoadAssembly();
            try
            {
                if (this.Assembly != null)
                {
                  object[] customAttributes = this.Assembly.GetCustomAttributes(false);
                    for (num = 1; num <= customAttributes.Length; num++)
                    {
                        object custattr = customAttributes[num - 1];
                        Type type = custattr.GetType();
                        switch (custattr.ToString())
                        {
                            case "Vulcan.Internal.VulcanClassLibraryAttribute":
                                this._GlobalClassName = type.GetProperty("globalClassName").GetValue(custattr, null).ToString();
                                //
                                string defaultNS = type.GetProperty("defaultNamespace").GetValue(custattr, null).ToString();
                                if ( !string.IsNullOrEmpty(defaultNS ))
                                    this._ImplicitNamespaces.Add(defaultNS);
                                break;
                            case "System.Runtime.CompilerServices.ExtensionAttribute":
                                this.lHasExtensions = true;
                                break;
                            case "Vulcan.VulcanImplicitNamespaceAttribute":
                                string nameS = type.GetProperty("Namespace").GetValue(custattr, null).ToString();
                                if (!string.IsNullOrEmpty(nameS))
                                    this._ImplicitNamespaces.Add(nameS);
                                break;

                        }
                    }
                }
            }
            catch
            {
            }
            // Load Types From Assembly, if possible
            // register event handler to find missing assemblies
            AppDomain currentDomain = AppDomain.CurrentDomain;
            currentDomain.AssemblyResolve += CurrentDomain_AssemblyResolve;
            try
            {
                if (this.Assembly != null)
                {
                    types = this.Assembly.GetTypes();
                }
            }
            catch (ReflectionTypeLoadException e)
            {
                Support.Debug("Cannot load types from {0}", Assembly.GetName().Name);
                Support.Debug("Exception details:");
                foreach (var le in e.LoaderExceptions)
                {
                    Support.Debug(le.Message);
                }
                Support.Debug("Types loaded:");
                foreach (var t in e.Types)
                {
                    Support.Debug(t.FullName);
                }
                this.Assembly = null;
            }
            catch (Exception e)
            {
                Support.Debug("Generic exception:");
                Support.Debug(e.Message);
            }
            // Has Types ?
            currentDomain.AssemblyResolve -= CurrentDomain_AssemblyResolve;
            if (types?.Length > 0 && (aTypes?.Count == 0  | ! lLoadedTypes))
            {
                try
                {
                    for (num = 1; num <= types.Length; num++)
                    {
                        // First, Get Fullname ( for eg, System.Collections.Generic.List`1 )
                        fullName = types[num - 1].FullName;
                        // Remove "special" Types
                        if (fullName.StartsWith("$") || fullName.StartsWith("<"))
                            continue;
                        //
                        if (this.lHasExtensions && HasExtensionAttribute(types[num - 1]))
                        {
                            MethodInfo[] methods = types[num - 1].GetMethods(BindingFlags.Public | BindingFlags.Static);
                            foreach (MethodInfo info in methods)
                            {
                                if (HasExtensionAttribute(info))
                                {
                                    aExtensions.Add(info);
                                }
                            }
                        }
                        // Nested Type ?
                        if (fullName.Contains("+"))
                        {
                            fullName = fullName.Replace('+', '.');
                        }
                        // Generic ?
                        if (fullName.Contains("`"))
                        {
                            // Extract the "normal" name
                            fullName = fullName.Substring(0, fullName.IndexOf("`") + 2);
                        }
                        // Add to the FullyQualified name
                        if (!aTypes.ContainsKey(fullName))
                        {
                            aTypes.Add(fullName, types[num - 1]);
                        }
                        // Now, with Standard name
                        simpleName = types[num - 1].Name;
                        simpleName = simpleName.Replace('+', '.');
                        // Not Empty namespace, not a generic, not nested, not starting with underscore
                        if (((string.IsNullOrEmpty(types[num - 1].Namespace) && (simpleName.IndexOf('`') == -1)) && ((simpleName.IndexOf('+') == -1) && (simpleName.IndexOf('<') == -1))) && !simpleName.StartsWith("_"))
                        {
                            // Add the Name only, with the Kind
                            this._ZeroNamespace.Types.Add(simpleName, this.GetTypeTypesFromType(types[num - 1]));
                        }
                        // Public Type, not Nested and no Underscore
                        if ((types[num - 1].IsPublic && (simpleName.IndexOf('+') == -1)) && (simpleName.IndexOf('_') == -1))
                        {
                            // Get the Namespace
                            nspace = types[num - 1].Namespace;
                            // and the normal name
                            simpleName = types[num - 1].Name;
                            simpleName = simpleName.Replace('+', '.');
                            // Generic ?
                            int index = simpleName.IndexOf('`');
                            if (index != -1)
                            {
                                simpleName = simpleName.Substring(0, index);
                            }
                            if ((nspace != null) && (nspace.Length > 0))
                            {
                                NameSpaceContainer container;
                                ;
                                if (!this._NameSpaces.ContainsKey(nspace))
                                {
                                    container = new NameSpaceContainer(nspace);
                                    container.Types.Add(simpleName, this.GetTypeTypesFromType(types[num - 1]));
                                    this._NameSpaces.Add(nspace, container);
                                    //this._NameSpaceTexts.Add(nspace + ".");
                                    this._NameSpaceTexts.Add(nspace);
                                }
                                else
                                {
                                    container = (NameSpaceContainer)this._NameSpaces[nspace];
                                    if (!container.Types.ContainsKey(simpleName))
                                    {
                                        container.Types.Add(simpleName, this.GetTypeTypesFromType(types[num - 1]));
                                    }
                                }
                                while (nspace.Contains("."))
                                {
                                    nspace = nspace.Substring(0, nspace.LastIndexOf('.'));
                                    if (!this._NameSpaceTexts.Contains(nspace))
                                    {
                                        this._NameSpaceTexts.Add(nspace);
                                    }
                                }
                            }
                        }
                    }
                    // Mark as Loaded
                    this.lLoadedTypes = true;
                    this._aTypes = aTypes.ToImmutableDictionary(StringComparer.OrdinalIgnoreCase);
                    this._aExtensions = aExtensions.ToImmutableList();
                }
                catch
                {
                    // empty values
                    this.lLoadedTypes = false;
                    aTypes = new Dictionary<string, Type>(StringComparer.OrdinalIgnoreCase);
                    this._aTypes = aTypes.ToImmutableDictionary();
                    this._aExtensions = new List<MethodInfo>();
                }
            }
        }

        private Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            var folders = new List<String>();   // list of folders that we have tried
            string folderPath = System.IO.Path.GetDirectoryName(this.FileName);
            var name = new AssemblyName(args.Name).Name + ".dll";
            string assemblyPath = Path.Combine(folderPath, name);
            if (File.Exists(assemblyPath))
            {
                var asm = LoadAssemblyFromFile(assemblyPath);
                if (asm != null)
                    return asm;
            }
            folders.Add(folderPath);
            // try in the folder for the other assemblies
            foreach (var path in SystemTypeController.AssemblyFileNames)
            {
                folderPath = System.IO.Path.GetDirectoryName(path);
                if (! folders.Contains(folderPath))
                {
                    assemblyPath = Path.Combine(folderPath, name);
                    if (File.Exists(assemblyPath))
                    {
                        var asm = Assembly.LoadFrom(assemblyPath);
                        if (asm != null)
                            return asm;
                    }
                    folders.Add(folderPath);
                }
            }


            return null;
        }

        private static bool HasExtensionAttribute(MemberInfo oInfo)
        {
            bool flag = false;
            string str = "";
            try
            {
                object[] customAttributes = oInfo.GetCustomAttributes(false);
                for (int i = 1; i <= customAttributes.Length; i++)
                {
                    str = customAttributes[i - 1].ToString();
                    if (str == "System.Runtime.CompilerServices.ExtensionAttribute")
                    {
                        flag = true;
                        break;
                    }
                }
            }
            catch
            {
            }
            return flag;
        }

        private TypeTypes GetTypeTypesFromType(Type oType)
        {
            if (oType.IsValueType)
            {
                return TypeTypes.Structure;
            }
            if (oType.IsInterface)
            {
                return TypeTypes.Interface;
            }
            if (typeof(Delegate).IsAssignableFrom(oType))
            {
                return TypeTypes.Delegate;
            }
            return TypeTypes.Class;
        }

        public enum TypeTypes
        {
            All = 0xff,
            Class = 1,
            Delegate = 8,
            Interface = 4,
            None = 0,
            Structure = 2
        }

        public class NameSpaceContainer
        {
            // Fields
            public SortedList<string, TypeTypes> Types;
            public string _NameSpace = "";

            // Methods
            public NameSpaceContainer(string _cNameSpace)
            {
                this._NameSpace = _cNameSpace;
                this.Types = new SortedList<string, TypeTypes>();
            }
        }
    }
}
