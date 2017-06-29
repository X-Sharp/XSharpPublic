//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using System.Collections.Immutable;

namespace XSharpModel
{
    /// <summary>
    /// We have one SystemTypeController in memory : It will handle all references types for all projects
    /// Assemblies are stored inside a List of AssemblyInfo
    /// </summary>
    public class SystemTypeController
    {
        internal static Dictionary<String, AssemblyInfo> assemblies;
        internal static Assembly mscorlib ;

        static SystemTypeController()
        {
            assemblies = new Dictionary<string, AssemblyInfo>(StringComparer.OrdinalIgnoreCase);
            mscorlib = null;
        }

        public static string FindAssemblyLocation(string fullName)
        {
            foreach (var pair in assemblies)
            {
                var asm = pair.Value;
                if (asm.Assembly != null && string.Compare(asm.Assembly.FullName, fullName, StringComparison.OrdinalIgnoreCase) == 0)
                {
                    return pair.Key; ;
                }
            }
            return null;

        }

        public static Assembly FindAssembly(string fullName)
        {
            foreach (var asm in assemblies.Values)
            {
                if (asm.Assembly != null && string.Compare(asm.Assembly.FullName, fullName, StringComparison.OrdinalIgnoreCase) == 0)
                    return asm.Assembly;
            }
            return null;
        }
        public static AssemblyInfo LoadAssembly(VSLangProj.Reference reference)
        {
            string path = reference.Path;
            if (String.IsNullOrEmpty(path))
            {
                return new AssemblyInfo(reference);
            }
            return LoadAssembly(path);
        }
        public static AssemblyInfo LoadAssembly(string cFileName)
        {
            DateTime lastWriteTime = File.GetLastWriteTime(cFileName);
            AssemblyInfo assembly;
            if (assemblies.ContainsKey(cFileName))
            {
                assembly = assemblies[cFileName];
            }
            else
            {
                assembly = new AssemblyInfo(cFileName, DateTime.MinValue);
                assemblies.Add(cFileName, assembly);
            }
            if (cFileName.EndsWith("mscorlib.dll",StringComparison.OrdinalIgnoreCase))
            {
                mscorlib = assembly.Assembly;
            }
            if (lastWriteTime != assembly.Modified)
            {
                assembly.UpdateAssembly();
            }
            if (Path.GetFileName(cFileName).ToLower() == "system.dll")
            {
                var mscorlib = Path.Combine(Path.GetDirectoryName(cFileName), "mscorlib.dll");
                if (! assemblies.ContainsKey(mscorlib) && File.Exists(mscorlib))
                {
                    LoadAssembly(mscorlib);
                }
            }
            return assembly;
        }

        public static void RemoveAssembly(string cFileName)
        {
            if (assemblies.ContainsKey(cFileName))
            {
                assemblies.Remove(cFileName);
            }
        }

        public Type FindType(string typeName, IReadOnlyList<string> usings, IReadOnlyList<AssemblyInfo> assemblies)
        {
            // generic types
            if (typeName.EndsWith(">"))
            {
                bool nested = typeName.Length > typeName.Replace(">", "").Length + 1;
                if (! nested)
                {
                    string[] elements = typeName.Split("<,>".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                    typeName = elements[0] + "`" + (elements.Length - 1).ToString();
                }
                else
                {
                    // something like Dictionary< String, Tuple<int, int> > 
                    int pos = typeName.IndexOf("<");
                    string baseName = typeName.Substring(0, pos);
                    // remove the outer "<" and ">", so we have String, Tuple<int, int> left
                    string typeParams = typeName.Substring(pos +1);
                    typeParams = typeParams.Substring(0, typeParams.Length - 1).Trim();
                    pos = typeParams.IndexOf("<");
                    while (pos >= 0)
                    {
                        int pos2 = typeParams.LastIndexOf(">");
                        // remove the type Params of the parameter
                        typeParams = typeParams.Substring(0, pos) + typeParams.Substring(pos2 + 1);
                        typeParams = typeParams.Trim();
                        pos = typeParams.IndexOf("<");
                    }
                    // now we have left String, Tuple
                    string[] elements = typeParams.Split(",".ToCharArray());
                    typeName = baseName + "`" + elements.Length.ToString();
                }
            }
            Type result = Lookup(typeName,assemblies);
            if (result != null)
                return result;
            // try to find with explicit usings
            foreach (var name in usings)
            {
                var fullname = name + "." + typeName;
                result = Lookup(fullname, assemblies);
                if (result != null)
                    return result;
            }
            // try to find with implicit namespaces
            foreach (var asm in assemblies)
            {
                foreach (var ns in asm.ImplicitNamespaces)
                {
                    var fullname = ns + "." + typeName;
                    result = Lookup(fullname, assemblies);
                    if (result != null)
                        return result;

                }
            }
            return null;
        }

        public static Type Lookup(string typeName, IReadOnlyList<AssemblyInfo> theirassemblies)
        {
            System.Type sType = null;
            foreach (AssemblyInfo assembly in theirassemblies)
            {
                //
                if (assembly.Types.Count == 0)
                {
                    assembly.UpdateAssembly();
                }
                if (assembly.Types.TryGetValue(typeName, out sType))
                {
                    break;
                }
                if (assembly.Assembly != null)
                {
                    sType = assembly.Assembly.GetType(typeName);
                }
                if (sType != null)
                {
                    break;
                }
            }
            if (sType == null)
            {
                // check mscorlib 
                if (mscorlib != null)
                {
                    sType = mscorlib.GetType(typeName);
                }
            }
            return sType;
        }

        public ImmutableList<String> GetNamespaces (IList<AssemblyInfo> assemblies)
        {
            List<string> result = new List<string>();
            //
            foreach (AssemblyInfo assembly in assemblies)
            {
                foreach (var ns in assembly.Namespaces)
                {
                    result.AddUnique(ns);
                }
            }
            return result.ToImmutableList();
        }

        public ImmutableList<AssemblyInfo> Assemblies
        {
            get
            {
                return assemblies.Values.ToImmutableList();
            }
        }

        public static void Clear()
        {
            assemblies.Clear();
        }

    }
}
