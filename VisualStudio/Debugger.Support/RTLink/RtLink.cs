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
        public const string ErrorPrefix = "XSharp Debugger Error : ";
        const string RTNotLoaded = ErrorPrefix+" XSharp Runtime not Loaded";
        const string TypeNotFound = ErrorPrefix + "Type '{0}' not found";
        const string MethodNotFound = ErrorPrefix + "Method '{0}' not found";
        const string PropertyNotFound = ErrorPrefix + "Property '{0}' not found";
        const string AssemblyNotFound = ErrorPrefix + "Assembly '{0}' not found";
        const string CouldNotReadState = ErrorPrefix + "Could not read the RuntimeState";
        const string CouldNotReadSettings = ErrorPrefix + "Could not read the Settings in the RuntimeState";
        const string CouldNotReadDataSession = ErrorPrefix + "Could not read the DataSession in the RuntimeState";
        const string ExceptionOccurred = ErrorPrefix + "Exception occurred during reading Runtime Info: {0}";
        static HashSet<Assembly> LoadedAssemblies = new HashSet<Assembly>();
        static Type memvarType = null;
        static Type globalsType = null;
        static Type stateType = null;
        public static readonly BindingFlags BFPublicStatic = BindingFlags.Public | BindingFlags.Static | BindingFlags.IgnoreCase;
        public static readonly BindingFlags BFPublicInstance= BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase;

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

        #region Find Types and Members
        static Type FindType(string name, string asmName, out string error)
        {
            error = null;
            var asm = LoadedAssemblies.First(a => a.FullName.ToLower().Contains(asmName.ToLower()));
            if (asm == null)
            {
                error = string.Format(AssemblyNotFound, asmName);
                return null;
            }
            var type = asm.GetType(name);
            if (type == null)
            {
                error = string.Format(TypeNotFound, name);
            }
            return type;
        }
        static MethodInfo FindMethod(this Type type, string Name, BindingFlags flags, out string error)
        {
            error = "";
            var mi = type.GetMethod(Name, flags);
            if (mi == null)
            {
                error = string.Format(MethodNotFound, Name) ;
            }
            return mi;
        }
        static PropertyInfo FindProperty(this Type type, string Name, BindingFlags flags, out string error)
        {
            error = "";
            var pi = type.GetProperty(Name,flags);
            if (pi == null)
            {
                error = string.Format(PropertyNotFound, Name);
            }
            return pi;
        }
        #endregion
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
                    globalsType = FindType("XSharp.Globals", "XSharp.RT", out error);

                    if (globalsType == null)
                        return error;
                }
                var mi = globalsType.FindMethod("GetAllGlobals", BFPublicStatic, out error);
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
                return string.Format(ExceptionOccurred, e.ToString());
            }
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
                    memvarType = FindType("XSharp.MemVar", "XSharp.RT", out error);
                    if (memvarType == null)
                        return error;
                }
                var mi1 = memvarType.FindMethod("DbgPublicsFirst", BFPublicStatic, out error);
                if (mi1 == null)
                    return error;
                var mi2 = memvarType.FindMethod("DbgPublicsNext", BFPublicStatic, out error);
                if (mi2 == null)
                    return error;
                var mi3 = memvarType.FindMethod("DbgGetVar", BFPublicStatic, out error);
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
                mi1 = memvarType.FindMethod("DbgPrivatesFirst", BFPublicStatic, out error);
                if (mi1 == null)
                    return error;
                mi2 = memvarType.FindMethod("DbgPrivatesNext", BFPublicStatic, out error);
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
                return string.Format(ExceptionOccurred, e.ToString());
            }

        }

        public static object GetState( out string error)
        {
            if (stateType == null)
            {
                stateType = FindType("XSharp.RuntimeState", "XSharp.Core", out error);
                if (stateType == null)
                    return null;
            }
            var mi1 = stateType.FindMethod( "GetInstance", BFPublicStatic, out error);
            if (mi1 == null)
                return error;
            var state = mi1.Invoke(null, null);
            if (state == null)
                error = CouldNotReadState;
            return state;
        }
        public static string GetSettings()
        {
            try
            {
                if (!IsRTLoaded())
                    return RTNotLoaded;
                string error;
                
               
                var state = GetState(out error);
                if (state == null)
                    return error;
                var prop = stateType.FindProperty("Settings", BFPublicInstance, out error);
                if (prop == null)
                    return error;
                var settings = prop.GetValue(state,null);
                if (settings == null)
                    return CouldNotReadSettings;
                var result = new SettingItems();
                foreach (var item in (IEnumerable) settings)
                {
                    // use dynamic here. The type is too complicated to resolve here.
                    dynamic kvp = item;
                    var first = kvp.Key;
                    var second = kvp.Value;
                    var setting = new SettingsItem();
                    setting.Key = (int)first;
                    setting.Name = first != null ? first.ToString() : "";
                    setting.Value = second != null ? second.ToString() : "";
                    result.Add(setting);
                }
                return result.Serialize();
            }
            catch (Exception e)
            {
                return string.Format(ExceptionOccurred, e.ToString());
            }
        }
        public static string GetWorkareas()
        {
            try
            {
                if (!IsRTLoaded())
                    return RTNotLoaded;
                string error;
                var state = GetState(out error);
                if (state == null)
                    return error;
                var prop = FindProperty(stateType, "CurrentWorkarea", BFPublicStatic, out error);
                if (prop == null)
                    return error;
                var selectedArea = (uint)prop.GetValue(null, null);
                prop = stateType.FindProperty( "DataSession", BFPublicStatic, out error);
                if (prop == null)
                    return error;
                var was = prop.GetValue(null, null);
                if (was == null)
                    return CouldNotReadDataSession;
                var type = was.GetType();
                var propRDDs = type.FindProperty("OpenRDDs",BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.FlattenHierarchy, out error);
                if (propRDDs == null)
                    return error;
                var rdds = propRDDs.GetValue(was, null);
                var result = new WorkareaItems();
                foreach (var item in (IEnumerable)rdds)
                {
                    // use dynamic here. The type is too complicated to resolve here.
                    dynamic kvp = item;
                    var first = kvp.Key;    // DWORD
                    dynamic second = kvp.Value; // IRdd
                    var waitem = new WorkareaItem();
                    waitem.Area = (int) first;
                    waitem.Alias = second.Alias;
                    waitem.RDD = second.Driver;
                    waitem.Selected = waitem.Area == (int)selectedArea;
                    result.Add(waitem);
                }
                return result.Serialize();
            }
            catch (Exception e)
            {
                return string.Format(ExceptionOccurred, e.ToString());
            }
        }
    }
}
