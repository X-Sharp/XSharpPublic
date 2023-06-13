using Debugger.Support;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace XSharp.Debugger.Support
{
    public static class RtLink
    {
        public const string ErrorPrefix = "XSharp Debugger Error:";
        const string RTNotLoaded = ErrorPrefix+"RT not Loaded";
        const string TypeNotFound = ErrorPrefix + "Type {0} not found";
        const string MethodNotFound = ErrorPrefix + "Method {0} not found";
        const string PropertyNotFound = ErrorPrefix + "Property {0} not found";
        const string AssemblyNotFound = ErrorPrefix + "Assembly {0} not found";
        const string CouldNotReadState = ErrorPrefix + "Could not read the RuntimeState";
        const string CouldNotReadSettings = ErrorPrefix + "Could not read the Settings in the RuntimeState";
        const string ExceptionOccurred = ErrorPrefix + "Exception occurred during reading Runtime Info {0}";
        static HashSet<Assembly> LoadedAssemblies = new HashSet<Assembly>();
        static Type memvarType = null;
        static Type globalsType = null;
        static Type stateType = null;

        static RtLink()
        {
            AppDomain.CurrentDomain.AssemblyLoad += AssemblyLoadEventHandler;
            var loadedAssemblies = new HashSet<Assembly>(AppDomain.CurrentDomain.GetAssemblies());
            do
            {
                LoadedAssemblies.UnionWith(loadedAssemblies);
                loadedAssemblies = new HashSet<Assembly>(AppDomain.CurrentDomain.GetAssemblies());
                loadedAssemblies.RemoveWhere(a => a.IsDynamic);
                loadedAssemblies.ExceptWith(LoadedAssemblies);
            } while (loadedAssemblies.Count > 0);

        }

        private static void AssemblyLoadEventHandler(object sender, AssemblyLoadEventArgs args)
        {
            lock (LoadedAssemblies)
            {
                if (!args.LoadedAssembly.IsDynamic && !LoadedAssemblies.Contains(args.LoadedAssembly))
                {
                    LoadedAssemblies.Add(args.LoadedAssembly);
                }
            }
        }

        public static bool IsRTLoaded()
        {
            return LoadedAssemblies.Any(a => a.FullName.ToLower().Contains("xsharp.rt")) &&
                LoadedAssemblies.Any(a => a.FullName.ToLower().Contains("xsharp.core"));
        }

        public static string GetGlobals()
        {
            try
            {
                if (!IsRTLoaded())
                    return RTNotLoaded;
                string error;
                if (globalsType == null)
                {
                    globalsType = FindType("XSharp.Globals", "xsharp.rt", out error);

                    if (globalsType == null)
                        return error;
                }
                var mi = FindMethod(globalsType, "GetAllGlobals", out error);
                if (mi == null)
                    return error;
                var fields = mi.Invoke(null, null) as IList<FieldInfo>;
                var result = new GlobalItems();
                foreach (FieldInfo fi in fields)
                {
                    GlobalItem item = new GlobalItem();
                    item.Name = fi.Name;
                    item.Assembly = fi.DeclaringType.Assembly.GetName().Name;
                    var value = fi.GetValue(null);
                    if (value != null)
                    {
                        item.Value = value.ToString();
                    }
                    else
                        item.Value = "<empty>";
                    result.Add(item);
                }

                return result.Serialize();
            }
            catch (Exception e)
            {
                return String.Format(ExceptionOccurred, e.ToString());
            }
        }

        static Type FindType(string name, string asmName, out string error)
        {
            error = null;
            var asm = LoadedAssemblies.First(a => a.FullName.ToLower().Contains(asmName));
            if (asm == null)
            {
                error = String.Format(AssemblyNotFound, asmName);
                return null;
            }
            var type = asm.GetType(name);
            if (type == null)
            {
                error = String.Format(TypeNotFound, name);
            }
            return type;
        }
        static MethodInfo FindMethod(Type type, string Name, out string error)
        {
            error = "";
            var mi = type.GetMethod(Name);
            if (mi == null)
            {
                error = String.Format(MethodNotFound, Name);
            }
            return mi;
        }
        static PropertyInfo FindProperty(Type type, string Name, out string error)
        {
            error = "";
            var pi = type.GetProperty(Name);
            if (pi == null)
            {
                error = String.Format(PropertyNotFound, Name);
            }
            return pi;
        }
        static public string GetMemVars()
        {
            try
            {
                if (!IsRTLoaded())
                    return RTNotLoaded;
                string error;
                if (memvarType == null)
                {
                    memvarType = FindType("XSharp.MemVar", "xsharp.rt", out error);
                    if (memvarType == null)
                        return error;
                }
                var mi1 = FindMethod(memvarType, "DbgPublicsFirst", out error);
                if (mi1 == null)
                    return error;
                var mi2 = FindMethod(memvarType, "DbgPublicsNext", out error);
                if (mi2 == null)
                    return error;
                var mi3 = FindMethod(memvarType, "DbgGetVar", out error);
                if (mi3 == null)
                    return error;

                var name = mi1.Invoke(null, null);
                var result = new MemvarItems();
                while (name != null)
                {
                    var value = mi3.Invoke(null, new object[] { name });
                    if (value == null)
                        value = "";
                    result.Add(new MemvarItem() { Name = name.ToString(), Type = MemvarType.Public, Value = value.ToString() });
                    name = mi2.Invoke(null, null);
                }
                mi1 = FindMethod(memvarType, "DbgPrivatesFirst", out error);
                if (mi1 == null)
                    return error;
                mi2 = FindMethod(memvarType, "DbgPrivatesNext", out error);
                if (mi2 == null)
                    return error;
                name = mi1.Invoke(null, null);
                while (name != null)
                {
                    var value = mi3.Invoke(null, new object[] { name });
                    if (value == null)
                        value = "";
                    result.Add(new MemvarItem() { Name = name.ToString(), Type = MemvarType.Private, Value = value.ToString() });
                    name = mi2.Invoke(null, null);
                }
                return result.Serialize();

            }
            catch (Exception e)
            {
                return String.Format(ExceptionOccurred, e.ToString());
            }

        }
        public static string GetSettings()
        {
            try
            {
                if (!IsRTLoaded())
                    return RTNotLoaded;
                string typeName = "XSharp.RuntimeState";
                string error;
                if (stateType == null)
                {
                    stateType = FindType(typeName, "xsharp.core", out error);
                    if (stateType == null)
                        return error;
                }
                var mi1 = FindMethod(stateType, "GetInstance", out error);
                if (mi1 == null)
                    return error;
                var prop = FindProperty(stateType, "Settings", out error);
                if (prop == null)
                    return error;
                var state = mi1.Invoke(null, null);
                if (state == null)
                    return CouldNotReadState;
                var propValue = prop.GetValue(state);
                if (propValue == null)
                    return CouldNotReadSettings;
                var dict = (IEnumerable)propValue;
                var result = new SettingItems();
                foreach (var item in dict)
                {
                    dynamic kvp = item;
                    var first = kvp.Key;
                    var second = kvp.Value;
                    var setting = new SettingsItem();
                    setting.Name = first != null ? first.ToString() : "";
                    setting.Value = second != null ? second.ToString() : "";
                    result.Add(setting);
                }
                return result.Serialize();
            }
            catch (Exception e)
            {
                return String.Format(ExceptionOccurred, e.ToString());
            }
        }
    }
}
