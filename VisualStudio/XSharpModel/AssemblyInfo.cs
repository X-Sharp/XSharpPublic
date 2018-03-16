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
        private IList<XProject> _projects;

        private string _fileName;
        private DateTime _Modified;
        private bool _LoadedTypes;
        private Assembly _assembly;
        private string _fullName = "";
        private string _globalClassName;
        private int _failed = 0;

        private Hashtable _nameSpaces;
        private List<string> _nameSpaceTexts;
        private List<string> _implicitNamespaces;
        private NameSpaceContainer _zeroNamespace;
        private VSLangProj.Reference _reference;

        // Has Extensions Methods ?
        private bool _HasExtensions;

        // clear info read from file
        private void _clearInfo()
        {
            _aTypes = new Dictionary<string, Type>(StringComparer.OrdinalIgnoreCase);
            _aExtensions = new List<MethodInfo>();
            _nameSpaces = new Hashtable(StringComparer.OrdinalIgnoreCase);
            _nameSpaceTexts = new List<string>();
            _implicitNamespaces = new List<string>();
            _zeroNamespace = new NameSpaceContainer("_");
            _LoadedTypes = false;
            _HasExtensions = false;
        }
        public string DisplayName
        {
            get
            {
                if (string.IsNullOrEmpty(_fileName))
                    return "(Empty)";
                return Path.GetFileName(_fileName);
            }
        }
        public string FileName
        {
            get
            {
                return _fileName;
            }

            internal set
            {
                _fileName = value;
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

        public string FullName => _fullName;
        public IDictionary<string, Type> Types
        {
            get
            {
                return _aTypes;
            }
        }

        public bool IsModifiedOnDisk
        {
            get
            {
                var currentTimestamp = File.GetLastWriteTime(_fileName);
                return currentTimestamp != Modified;
            }
        }
        public System.Type GetType(string name)
        {
            if (IsModifiedOnDisk)
            {
                LoadAssembly();
            }
            if (_assembly != null && _aTypes.Count == 0)
            {
                UpdateAssembly();
            }
            if (_aTypes.ContainsKey(name))
                return Types[name];
            return null;
        }
        public ImmutableList<string> ImplicitNamespaces => _implicitNamespaces.ToImmutableList();
        public String GlobalClassName => _globalClassName;

        public ImmutableList<string> Namespaces => _nameSpaceTexts.ToImmutableList();


        public AssemblyInfo()
        {
            this._assembly = null;
            this._projects = new List<XProject>();
            _clearInfo();
        }
        public AssemblyInfo (VSLangProj.Reference reference) : this()
        {
            _reference = reference;
        }
        public AssemblyInfo(string _cFileName, DateTime _dModified) : this()
        {
            this._fileName = _cFileName;
            this.Modified = _dModified;

            this.UpdateAssembly();
        }


        public static Assembly LoadAssemblyFromFile(string fileName)
        {
            if (!File.Exists(fileName))
            {
                return null;
            }
            try
            {
                FileStream input = new FileStream(fileName, FileMode.Open, FileAccess.Read);
                byte[] rawAssembly = new BinaryReader(input).ReadBytes((int)input.Length);
                if (rawAssembly.Length != input.Length)
                {
                    //MessageBox.Show("Intellisense error 9");
                }
                input.Close();

                // if the PDB file exists then this might put a lock on the pdb file.
                // so we rename the pdb temporarily to prevent the lock
                var cPdb = Path.ChangeExtension(fileName, ".pdb");
                var cTmp = Path.ChangeExtension(fileName, ".p$$");
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
            catch (Exception e)
            {
                Support.Debug("Generic exception:");
                Support.Debug(e.Message);
            }
            return null;
        }

        internal void LoadAssembly()
        {
            if (String.IsNullOrEmpty(_fileName))
            {
                if (_reference != null)
                {
                    _fileName = _reference.Path;
                }
            }
            if (!File.Exists(_fileName))
            {
                return ;
            }
            _assembly = LoadAssemblyFromFile(_fileName);
            _fullName = _assembly.FullName;
            this.Modified = File.GetLastWriteTime(_fileName);
            _clearInfo();
        }

        internal void UpdateAssembly()
        {
            if (this._failed > 3 )
                return;
            Type[] types = null;
            var aTypes = new Dictionary<string, Type>(StringComparer.OrdinalIgnoreCase);
            var aExtensions = new List<MethodInfo>();
            //
            int num;
            string nspace = "";
            string fullName = "";
            string simpleName = "";
            //
            this._nameSpaces.Clear();
            this._nameSpaceTexts.Clear();
            
            this._zeroNamespace.Clear();
            this._globalClassName = "";
            this._HasExtensions = false;
            //
            this.LoadAssembly();
            try
            {
                if (_assembly != null)
                {
                  object[] customAttributes = _assembly.GetCustomAttributes(false);
                    for (num = 1; num <= customAttributes.Length; num++)
                    {
                        object custattr = customAttributes[num - 1];
                        Type type = custattr.GetType();
                        switch (custattr.ToString())
                        {
                            case "Vulcan.Internal.VulcanClassLibraryAttribute":
                                this._globalClassName = type.GetProperty("globalClassName").GetValue(custattr, null).ToString();
                                //
                                string defaultNS = type.GetProperty("defaultNamespace").GetValue(custattr, null).ToString();
                                if ( !string.IsNullOrEmpty(defaultNS ))
                                    this._implicitNamespaces.Add(defaultNS);
                                break;
                            case "System.Runtime.CompilerServices.ExtensionAttribute":
                                this._HasExtensions = true;
                                break;
                            case "Vulcan.VulcanImplicitNamespaceAttribute":
                                string nameS = type.GetProperty("Namespace").GetValue(custattr, null).ToString();
                                if (!string.IsNullOrEmpty(nameS))
                                    this._implicitNamespaces.Add(nameS);
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
                if (_assembly != null)
                {
                    types = _assembly.GetTypes();
                }
                this._failed = 0;
            }
            catch (ReflectionTypeLoadException e)
            {
                Support.Debug("Cannot load types from {0}", _assembly.GetName().Name);
                Support.Debug("Exception details:");
                string lastMsg = null;
                foreach (var le in e.LoaderExceptions)
                {
                    if (le.Message != lastMsg)
                    {
                        Support.Debug(le.Message);
                        lastMsg = le.Message;
                    }
                }
                Support.Debug("Types loaded:");
                foreach (var t in e.Types)
                {
                    if (t != null)
                    {
                        Support.Debug(t.FullName);
                    }
                }
                _assembly = null;
                this._failed += 1;
            }
            catch (Exception e)
            {
                Support.Debug("Generic exception:");
                Support.Debug(e.Message);
            }
            // Has Types ?
            currentDomain.AssemblyResolve -= CurrentDomain_AssemblyResolve;
            if (types?.Length > 0 && (aTypes?.Count == 0  | ! _LoadedTypes))
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
                        if (this._HasExtensions && HasExtensionAttribute(types[num - 1]))
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
                            this._zeroNamespace.AddType(simpleName, this.GetTypeTypesFromType(types[num - 1]));
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
                                if (!this._nameSpaces.ContainsKey(nspace))
                                {
                                    container = new NameSpaceContainer(nspace);
                                    container.AddType(simpleName, this.GetTypeTypesFromType(types[num - 1]));
                                    this._nameSpaces.Add(nspace, container);
                                    this._nameSpaceTexts.Add(nspace);
                                }
                                else
                                {
                                    container = (NameSpaceContainer)this._nameSpaces[nspace];
                                    container.AddType(simpleName, this.GetTypeTypesFromType(types[num - 1]));
                                }
                                while (nspace.Contains("."))
                                {
                                    nspace = nspace.Substring(0, nspace.LastIndexOf('.'));
                                    if (!this._nameSpaceTexts.Contains(nspace))
                                    {
                                        this._nameSpaceTexts.Add(nspace);
                                    }
                                }
                            }
                        }
                    }
                    // Mark as Loaded
                    this._LoadedTypes = true;
                    this._aTypes = aTypes.ToImmutableDictionary(StringComparer.OrdinalIgnoreCase);
                    this._aExtensions = aExtensions.ToImmutableList();
                    this._failed = 0;
                    _assembly = null;
                }
                catch (Exception e)
                {
                    Support.Debug("Generic exception:");
                    Support.Debug(e.Message);
                    // empty values
                    _clearInfo();
                }
            }
        }

        private Assembly CurrentDomain_AssemblyResolve(object sender, ResolveEventArgs args)
        {
            var folders = new List<String>();   // list of folders that we have tried
            string folderPath = System.IO.Path.GetDirectoryName(this._fileName);
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

        internal class NameSpaceContainer
        {
            // Fields
            internal SortedList<string, TypeTypes> _Types;
            internal string _NameSpace = "";

            // Methods
            public NameSpaceContainer(string _cNameSpace)
            {
                this._NameSpace = _cNameSpace;
                this._Types = new SortedList<string, TypeTypes>();
            }

            public void Clear()
            {
                _Types.Clear();
            }
            public void AddType(string typeName, TypeTypes type)
            {
                if (! _Types.ContainsKey(typeName))
                {
                    _Types.Add(typeName, type);
                }
            }

        }
        public void AddProject(XProject project)
        {
            if (! _projects.Contains(project))
                _projects.Add(project);
        }

        public void RemoveProject(XProject project)
        {
            if (_projects.Contains(project))
            {
                _projects.Remove(project);
            }
        }
        public bool HasProjects => _projects.Count > 0;

    }
}
