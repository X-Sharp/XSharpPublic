using Debugger.Support;

using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
using System.Net;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Security.Cryptography;


namespace XSharp.Debugger.Support
{
    public static class RtLink
    {
        public const string ErrorPrefix = "XSharp Debugger Error : ";
        const string ErrorValue = "** Error **";
        const string RTNotLoaded = ErrorPrefix + " XSharp Runtime not Loaded";
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
        public static readonly BindingFlags BFPublicInstance = BindingFlags.Public | BindingFlags.Instance | BindingFlags.IgnoreCase;
        public static readonly BindingFlags BFPrivateInstance = BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.FlattenHierarchy;

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
                error = string.Format(MethodNotFound, Name);
            }
            return mi;
        }
        static PropertyInfo FindProperty(this Type type, string Name, BindingFlags flags, out string error)
        {
            error = "";
            var pi = type.GetProperty(Name, flags);
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

        public static object GetState(out string error)
        {
            if (stateType == null)
            {
                stateType = FindType("XSharp.RuntimeState", "XSharp.Core", out error);
                if (stateType == null)
                    return null;
            }
            var mi1 = stateType.FindMethod("GetInstance", BFPublicStatic, out error);
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
                var settings = prop.GetValue(state, null);
                if (settings == null)
                    return CouldNotReadSettings;
                var result = new SettingItems();
                foreach (var item in (IEnumerable)settings)
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
                prop = stateType.FindProperty("DataSession", BFPublicStatic, out error);
                if (prop == null)
                    return error;
                var was = prop.GetValue(null, null);
                if (was == null)
                    return CouldNotReadDataSession;
                var type = was.GetType();
                var propRDDs = type.FindProperty("OpenRDDs", BFPrivateInstance, out error);
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
                    waitem.Area = (int)first;
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

        public static object _GetArea(int areaNo)
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
            prop = stateType.FindProperty("DataSession", BFPublicStatic, out error);
            if (prop == null)
                return error;
            var was = prop.GetValue(null, null);
            if (was == null)
                return CouldNotReadDataSession;
            var type = was.GetType();
            var propRDDs = type.FindProperty("OpenRDDs", BFPrivateInstance, out error);
            if (propRDDs == null)
                return error;
            var rdds = propRDDs.GetValue(was, null);
            dynamic area = null;
            foreach (var item in (IEnumerable)rdds)
            {
                // use dynamic here. The type is too complicated to resolve here.
                dynamic kvp = item;
                var first = kvp.Key;    // DWORD
                if ((int)first == areaNo)
                {
                    area = kvp.Value;
                    break;
                }
            }
            return area;
        }

        public static string GetArea(int areaNo)
        {
            object area = _GetArea(areaNo);
            if (area != null)
            {
                var result = new NameValueItems { Sorted = true };
                var atype = area.GetType();
                var props = atype.GetProperties(BFPublicInstance).ToArray();
                foreach (var property in props)
                {
                    if (property.CanRead)
                    {
                        try
                        {
                            var value = property.GetValue(area, null);
                            if (value is null)
                            {
                                continue;
                            }
                            switch (value)
                            {
                                case string _:
                                case bool _:
                                case int _:
                                case uint _:
                                case long _:
                                case DateTime _:
                                case ulong _:
                                    result.Add(new NameValueItem { Name = property.Name, Value = value.ToString() });
                                    break;
                            }
                        }
                        catch
                        {
                            result.Add(new NameValueItem { Name = property.Name, Value = ErrorValue });
                        }
                    }

                }
                return result.Serialize();
            }
            return "";

        }
        public static string GetFieldValues(int areaNo)
        {
            dynamic area = _GetArea(areaNo);
            if (area != null)
            {
                var result = new NameValueItems();

                int fields = area.FieldCount;
                for (int i = 1; i <= fields; i++)
                {
                    string name = $"Field {i}";
                    object value;
                    try
                    {
                        dynamic info = area.GetField(i);
                        name = info.Name;
                        var Alias = info.Alias;
                        if (name != Alias)
                            name = Alias;
                        value = area.GetValue(i);
                        if (value is null)
                        {
                            value = "<NULL>";
                        }
                        result.Add(new NameValueItem { Name = name+" ", Value = value.ToString() });
                    }
                    catch
                    {
                        result.Add(new NameValueItem { Name = name + " ", Value = ErrorValue });
                    }
                }
                return result.Serialize();
            }
            return "";
        }
        public static string GetStructure(int areaNo)
        {
            dynamic area = _GetArea(areaNo);
            if (area != null)
            {
                var result = new NameValueItems();
                int fields = area.FieldCount;

                for (int i = 1; i <= fields; i++)
                {
                    string Name = $"Field {i}";
                    try
                    {
                        dynamic info = area.GetField(i);
                        Name = info.Name;
                        var Alias = info.Alias;
                        if (Name != Alias)
                            Name = Alias;
                        var item = new NameValueItem { Name = Name + " ", Value = info.FieldType.ToString()+" ", Length = info.Length.ToString() };
                        if (info.Decimals != 0)
                        {
                            item.Decimals = info.Decimals.ToString();
                        }
                        result.Add(item);
                    }
                    catch
                    {
                        result.Add(new NameValueItem { Name = Name, Value = ErrorValue });
                    }
                }
                return result.Serialize();
            }
            return "";
        }
        public static string GetIndexes(int areaNo)
        {
            dynamic area = _GetArea(areaNo);
            if (area != null)
            {
                var result = new NameValueItems();
                string error;
                var type = FindType("XSharp.RDD.Support.DbOrderInfo", "XSharp.Core", out error);
                if (type == null)
                {
                    return error;
                }
                dynamic orderInfo = Activator.CreateInstance(type);
                area.OrderInfo((uint)DbOrder_Info.DBOI_ORDERCOUNT, orderInfo);
                int orderCount = Convert.ToInt32(orderInfo.Result);
                if (orderCount == 0)
                {
                    result.Add(new NameValueItem { Name = "Indexes", Value = "No indexes" });
                }
                string fileName = "";
                // Get the current selected index
                area.OrderInfo((uint)DbOrder_Info.DBOI_NUMBER, orderInfo);
                int currentIndex = Convert.ToInt32(orderInfo.Result);

                for (int i = 1; i <= orderCount; i++)
                {
                    string name = $"Index {i}";
                    try
                    {
                        orderInfo.Order = i;
                        area.OrderInfo((uint)DbOrder_Info.DBOI_FULLPATH, orderInfo);
                        name = orderInfo.Result.ToString();
                        if (name != fileName)
                        {
                            result.Add(new NameValueItem { Name = "FileName", Value = name });
                            fileName = name;
                        }
                        if (i == currentIndex)
                        {
                            result.Add(new NameValueItem { Name = "ACTIVE Order", Value = i.ToString() });
                        }
                        else
                        {
                            result.Add(new NameValueItem { Name = "Order", Value = i.ToString() });
                        }

                        area.OrderInfo((uint)DbOrder_Info.DBOI_NAME, orderInfo);
                        name = orderInfo.Result.ToString();
                        result.Add(new NameValueItem { Name = "Name", Value = name });

                        area.OrderInfo((uint)DbOrder_Info.DBOI_EXPRESSION, orderInfo);
                        name = orderInfo.Result.ToString();
                        result.Add(new NameValueItem { Name = "Expression", Value = name });

                        area.OrderInfo((uint)DbOrder_Info.DBOI_CONDITION, orderInfo);
                        name = orderInfo.Result.ToString();
                        if (!string.IsNullOrEmpty(name))
                        {
                            result.Add(new NameValueItem { Name = "Condition", Value = name });
                        }
                        area.OrderInfo((uint)DbOrder_Info.DBOI_KEYCOUNT, orderInfo);
                        name = orderInfo.Result.ToString();
                        result.Add(new NameValueItem { Name = "KeyCount", Value = name });

                        area.OrderInfo((uint)DbOrder_Info.DBOI_KEYVAL, orderInfo);
                        name = orderInfo.Result.ToString();
                        result.Add(new NameValueItem { Name = "KeyValue", Value = name });


                        result.Add(new NameValueItem { Name = "", Value = "" });


                    }
                    catch
                    {
                        result.Add(new NameValueItem { Name = name, Value = ErrorValue });
                    }
                }

                return result.Serialize();
            }
            return "";
        }
    }
    enum DbOrder_Info : uint
    {
        DBOI_CONDITION = 1,
        DBOI_EXPRESSION = 2,
        DBOI_POSITION = 3,
        DBOI_KEYGOTO = 3,
        DBOI_KEYNO = 3,
        DBOI_RECNO = 4,
        DBOI_NAME = 5,
        DBOI_NUMBER = 6,
        DBOI_BAGNAME = 7,
        DBOI_INDEXNAME = 7,
        DBOI_BAGEXT = 8,
        DBOI_INDEXEXT = 8,
        DBOI_DEFBAGEXT = 9,
        DBOI_COLLATION = 10,
        DBOI_FULLPATH = 20,
        DBOI_FILEHANDLE = 21,
        DBOI_ISDESC = 22,
        DBOI_ISCOND = 23,
        DBOI_KEYTYPE = 24,
        DBOI_KEYSIZE = 25,
        DBOI_KEYCOUNT = 26,
        DBOI_SETCODEBLOCK = 27,
        DBOI_KEYDEC = 28,
        DBOI_HPLOCKING = 29,
        DBOI_LOCKOFFSET = 35,
        DBOI_KEYADD = 36,
        DBOI_KEYDELETE = 37,
        DBOI_KEYVAL = 38,
        DBOI_SCOPETOP = 39,
        DBOI_SCOPEBOTTOM = 40,
        DBOI_SCOPETOPCLEAR = 41,
        DBOI_SCOPEBOTTOMCLEAR = 42,
        DBOI_UNIQUE = 43,
        DBOI_ORDERCOUNT = 44,
        DBOI_CUSTOM = 45,
        DBOI_SKIPUNIQUE = 46,
        DBOI_KEYSINCLUDED = 48,
        DBOI_KEYNORAW = 49,
        DBOI_OPTLEVEL = 50,
        DBOI_KEYCOUNTRAW = 51,
        DBOI_FILESTREAM = 52,
        DBOI_STRICTREAD = 60,
        DBOI_OPTIMIZE = 61,
        DBOI_AUTOOPEN = 62,
        DBOI_AUTOORDER = 63,
        DBOI_AUTOSHARE = 64,
        DBOI_LOCK_ALL = 100,
        DBOI_LOCK_FAIL = 101,
        DBOI_HPLOCK_GATE = 102,
        DBOI_SKIPEVAL = 200,
        DBOI_SKIPEVALBACK = 201,
        DBOI_SKIPREGEX = 202,
        DBOI_SKIPREGEXBACK = 203,
        DBOI_SKIPWILD = 204,
        DBOI_SKIPWILDBACK = 205,
        DBOI_SCOPEEVAL = 206,
        DBOI_FINDREC = 207,
        DBOI_FINDRECCONT = 208,
        DBOI_SCOPESET = 209,
        DBOI_SCOPECLEAR = 210,
        DBOI_BAGCOUNT = 211,
        DBOI_BAGNUMBER = 212,
        DBOI_BAGORDER = 213,
        DBOI_ISMULTITAG = 214,
        DBOI_ISSORTRECNO = 215,
        DBOI_LARGEFILE = 216,
        DBOI_TEMPLATE = 217,
        DBOI_MULTIKEY = 218,
        DBOI_CHGONLY = 219,
        DBOI_PARTIAL = 220,
        DBOI_SHARED = 221,
        DBOI_ISREADONLY = 222,
        DBOI_READLOCK = 223,
        DBOI_WRITELOCK = 224,
        DBOI_UPDATECOUNTER = 225,
        DBOI_EVALSTEP = 226,
        DBOI_ISREINDEX = 227,
        DBOI_I_BAGNAME = 228,
        DBOI_I_TAGNAME = 229,
        DBOI_RELKEYPOS = 230,
        DBOI_USECURRENT = 231,
        DBOI_INDEXTYPE = 232,
        DBOI_RESETPOS = 233,
        DBOI_INDEXPAGESIZE = 234,
        DBOI_DUMP = 300,
        DBOI_VALIDATE = 301,
        DBOI_USER = 1000,
        DBOI_AXS_PERCENT_INDEXED = 1805,
        DBOI_GET_ACE_INDEX_HANDLE = 1806
    }
    public enum DbFieldInfo
    {
        DBS_NAME = 1,
        DBS_TYPE = 2,
        DBS_LEN = 3,
        DBS_DEC = 4,
        DBS_ALIAS = 5,
        DBS_FLAGS = 6,
        DBS_ISNULL = 11,
        DBS_COUNTER = 12,
        DBS_STEP = 13,
        DBS_CAPTION = 14,
        DBS_COLUMNINFO = 15,
        DBS_DESCRIPTION = 16,
        DBS_BLANK = 17,
        DBS_BLOB_GET = 101,
        DBS_BLOB_TYPE = 102,
        DBS_BLOB_LEN = 103,
        DBS_BLOB_OFFSET = 104,
        DBS_BLOB_POINTER = 198,
        DBS_BLOB_DIRECT_TYPE = 222,
        DBS_BLOB_DIRECT_LEN = 223,
        DBS_STRUCT = 998,
        DBS_PROPERTIES = 999,
        DBS_USER = 1000
    }

}
