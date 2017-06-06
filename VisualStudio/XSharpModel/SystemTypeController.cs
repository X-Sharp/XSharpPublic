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

        internal static Assembly RealLoadAssembly(string cFile)
        {
            Assembly assembly = null;
            if (!File.Exists(cFile))
            {
                return null;
            }
            try
            {
                FileStream input = new FileStream(cFile, FileMode.Open, FileAccess.Read);
                byte[] rawAssembly = new BinaryReader(input).ReadBytes((int)input.Length);
                if (rawAssembly.Length != input.Length)
                {
                    //MessageBox.Show("Intellisense error 9");
                }
                input.Close();

                // if the PDB file exists then this might put a lock on the pdb file.
                // so we rename the pdb temporarily to prevent the lock
                var cPdb = Path.ChangeExtension(cFile, ".pdb");
                var cTmp = Path.ChangeExtension(cFile, ".p$$");
                bool renamed = false;
                if (File.Exists(cPdb))
                {
                    renamed = true;
                    if (File.Exists(cTmp))
                        File.Delete(cTmp);
                    File.Move(cPdb, cTmp);
                }
                assembly = Assembly.Load(rawAssembly);
                if (renamed && File.Exists(cTmp))
                {
                    File.Move(cTmp, cPdb);
                }
                input.Dispose();
                rawAssembly = null;
            }
            catch
            {
            }
            return assembly;
        }

        public static AssemblyInfo LoadAssembly(string cFileName)
        {
            Assembly oAssembly = null;
            if (!File.Exists(cFileName))
            {
                return null;
            }
            DateTime lastWriteTime = File.GetLastWriteTime(cFileName);
            AssemblyInfo assembly;
            if (assemblies.ContainsKey(cFileName))
            {
                assembly = assemblies[cFileName];
            }
            else
            {
                assembly = new AssemblyInfo(cFileName, DateTime.MinValue, null);
            }
            if (lastWriteTime != assembly.Modified)
            {
                oAssembly = RealLoadAssembly(cFileName);
                if (oAssembly != null)
                {
                    assembly.Assembly = oAssembly;
                    assembly.Modified = lastWriteTime;
                    assembly.UpdateAssembly();
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
                if (assembly.Types.TryGetValue(typeName, out sType))
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
