using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public class AssemblyInfo
    {
        // Fields
        /// <summary>
        /// A Dictionnary of Type : The key is a string with the typeName in LowerCase
        /// </summary>
        private Dictionary<string, Type> aTypes;
        private List<MethodInfo> aExtensions;

        private string _DefaultNamespace;
        private string _FileName;
        private DateTime _Modified;
        private bool lLoadedTypes;
        private Assembly _Assembly;
        private string _GlobalClassName;

        private Hashtable _NameSpaces;
        private List<string> _NameSpaceTexts;
        private NameSpaceContainer _ZeroNamespace;


        // Has Extensions Methods ?
        private bool lHasExtensions;

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

        public Dictionary<string, Type> Types
        {
            get
            {
                return aTypes;
            }

            set
            {
                aTypes = value;
            }
        }

        public List<string> Namespaces
        {
            get
            {
                return _NameSpaceTexts;
            }
        }

        public AssemblyInfo(string _cFileName, DateTime _dModified, Assembly _oAssembly)
        {
            this._DefaultNamespace = "";
            this.FileName = _cFileName;
            this.Modified = _dModified;
            this.Assembly = _oAssembly;
            // A
            this.aTypes = new Dictionary<string, Type>();
            this.aExtensions = new List<MethodInfo>();
            this._NameSpaces = new Hashtable();
            this._NameSpaceTexts = new List<string>();
            this.aExtensions = new List<MethodInfo>();
            this._ZeroNamespace = new NameSpaceContainer("_");

            this.UpdateAssembly();
        }

        internal void UpdateAssembly()
        {
            Type[] types = null;
            //
            int num;
            string nspace = "";
            string fullName = "";
            string simpleName = "";
            string str3 = "";
            //
            this.aTypes.Clear();
            //
            this._NameSpaces.Clear();
            this._NameSpaceTexts.Clear();
            this.aExtensions.Clear();
            this._ZeroNamespace.Types.Clear();
            this._GlobalClassName = "";
            this._DefaultNamespace = "";
            this.lHasExtensions = false;
            //
            try
            {
                object[] customAttributes = this.Assembly.GetCustomAttributes(false);
                for (num = 1; num <= customAttributes.Length; num++)
                {
                    object obj2 = customAttributes[num - 1];
                    switch (obj2.ToString())
                    {
                        case "Vulcan.Internal.VulcanClassLibraryAttribute":
                            {
                                Type type = obj2.GetType();
                                this._GlobalClassName = type.GetProperty("globalClassName").GetValue(obj2, null).ToString();
                                this._DefaultNamespace = type.GetProperty("defaultNamespace").GetValue(obj2, null).ToString();
                                break;
                            }
                        case "System.Runtime.CompilerServices.ExtensionAttribute":
                            this.lHasExtensions = true;
                            break;
                    }
                }
            }
            catch
            {
            }
            // Load Types From Assembly, if possible
            try
            {
                types = this.Assembly.GetTypes();
            }
            catch
            {
            }
            // Has Types ?
            if (types != null)
            {
                // Mark as Loaded
                this.lLoadedTypes = true;
                try
                {
                    for (num = 1; num <= types.Length; num++)
                    {
                        // First, Get Fullname ( for eg, System.Collections.Generic.List`1 )
                        fullName = types[num - 1].FullName;
                        if (this.lHasExtensions && HasExtensionAttribute(types[num - 1]))
                        {
                            MethodInfo[] methods = types[num - 1].GetMethods(BindingFlags.Public | BindingFlags.Static);
                            foreach (MethodInfo info in methods)
                            {
                                if (HasExtensionAttribute(info))
                                {
                                    this.aExtensions.Add(info);
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
                        if (!this.aTypes.ContainsKey(fullName.ToLower()))
                        {
                            this.aTypes.Add(fullName.ToLower(), types[num - 1]);
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
                                str3 = nspace.ToLower();
                                if (!this._NameSpaces.ContainsKey(str3))
                                {
                                    container = new NameSpaceContainer(nspace);
                                    container.Types.Add(simpleName, this.GetTypeTypesFromType(types[num - 1]));
                                    this._NameSpaces.Add(str3, container);
                                    //this._NameSpaceTexts.Add(nspace + ".");
                                    this._NameSpaceTexts.Add(nspace );
                                }
                                else
                                {
                                    container = (NameSpaceContainer)this._NameSpaces[str3];
                                    if (!container.Types.ContainsKey(simpleName))
                                    {
                                        container.Types.Add(simpleName, this.GetTypeTypesFromType(types[num - 1]));
                                    }
                                }
                                while (nspace.Contains("."))
                                {
                                    nspace = nspace.Substring(0, nspace.LastIndexOf('.'));
                                    if (!this._NameSpaceTexts.Contains(nspace ))
                                    {
                                        this._NameSpaceTexts.Add(nspace);
                                    }
                                }
                            }
                        }
                    }
                }
                catch
                {
                }
            }
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
