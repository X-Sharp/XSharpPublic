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

namespace XSharpModel
{
    /// <summary>
    /// We have one SystemTypeController in memory : It will handle all references types for all projects
    /// Assemblies are stored inside a List of AssemblyInfo
    /// </summary>
    public class SystemTypeController
    {
        internal static Dictionary<String, AssemblyInfo> assemblies;

        static SystemTypeController()
        {
            assemblies = new Dictionary<string, AssemblyInfo>(StringComparer.OrdinalIgnoreCase);
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
            if (lastWriteTime != assembly.Modified)
            {
                assembly.UpdateAssembly();
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

        public Type FindType(string typeName, IList<string> usings, IList<AssemblyInfo> assemblies)
        {
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

        public static Type Lookup(string typeName, IList<AssemblyInfo> assemblies)
        {
            System.Type sType = null;
            foreach (AssemblyInfo assembly in assemblies)
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
                sType = assembly.Assembly.GetType(typeName);
                if (sType != null)
                {
                    break;
                }
            }
            return sType;
        }

        public List<String> GetNamespaces (IList<AssemblyInfo> assemblies)
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
            return result;
        }

        public IEnumerable<AssemblyInfo> Assemblies
        {
            get
            {
                return assemblies.Values;
            }
        }

        public static void Clear()
        {
            assemblies.Clear();
        }

    }
}
