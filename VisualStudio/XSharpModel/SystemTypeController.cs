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
        internal static List<AssemblyInfo> assemblies;

        static SystemTypeController()
        {
            assemblies = new List<AssemblyInfo>();
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

        public static Assembly LoadAssembly(string cFileName)
        {
            Assembly oAssembly = null;
            if (!File.Exists(cFileName))
            {
                return null;
            }
            cFileName = cFileName.ToLower();
            DateTime lastWriteTime = File.GetLastWriteTime(cFileName);
            for (int i = 0; i <= (assemblies.Count - 1); i++)
            {
                AssemblyInfo assembly2 = assemblies[i];
                if (assembly2.FileName == cFileName)
                {
                    if (lastWriteTime == assembly2.Modified)
                    {
                        oAssembly = assembly2.Assembly;
                    }
                    else
                    {
                        oAssembly = RealLoadAssembly(cFileName);
                        if (oAssembly != null)
                        {
                            assembly2.Assembly = oAssembly;
                            assembly2.Modified = lastWriteTime;
                            assembly2.UpdateAssembly();
                        }
                        else
                        {
                            oAssembly = assembly2.Assembly;
                        }
                    }
                    break;
                }
            }
            if (oAssembly == null)
            {
                oAssembly = RealLoadAssembly(cFileName);
                if (oAssembly != null)
                {
                    assemblies.Add(new AssemblyInfo(cFileName.ToLower(), lastWriteTime, oAssembly));
                }
            }
            return oAssembly;
        }

        public static void RemoveAssembly(string cFileName)
        {
            int FoundAt = -1;
            //
            for (int i = 0; i <= (assemblies.Count - 1); i++)
            {
                AssemblyInfo assembly2 = assemblies[i];
                if (assembly2.FileName == cFileName.ToLower())
                {
                    FoundAt = i;
                    break;
                }
            }
            // Remove, so it will not appears in NameSpace and LookUp
            if ( FoundAt > -1 )
                assemblies.RemoveAt(FoundAt);
        }

        public Type FindType(string typeName, IList<string> usings)
        {
            Type result = Lookup(typeName);
            if (result != null)
                return result;
            foreach (var name in usings)
            {
                var fullname = name + "." + typeName;
                result = Lookup(fullname);
                if (result != null)
                    return result;
            }
            return null;
        }

        public static Type Lookup(string typeName)
        {
            System.Type sType = null;
            // We are searching for the type name in lower case
            typeName = typeName.ToLowerInvariant();
            foreach (AssemblyInfo assembly in SystemTypeController.assemblies)
            {
                //
                if (assembly.Types.TryGetValue(typeName, out sType))
                {
                    break;
                }
            }
            return sType;
        }

        public List<String> Namespaces
        {
            get
            {
                List<string> ns = new List<string>();
                //
                foreach (AssemblyInfo assembly in SystemTypeController.assemblies)
                {
                    //
                    ns.AddRange(assembly.Namespaces);
                }
                return ns;
            }
        }

        public List<AssemblyInfo> Assemblies
        {
            get
            {
                return SystemTypeController.assemblies;
            }
        }

    }
}
