using System;
using System.CodeDom;
using System.Collections;
namespace XSharp.CodeDom
{
    public static class XSharpCodeConstants
    {
        public static string USERDATA_SOURCECODE = "XSharp:Sourcecode"; // string
        public static string USERDATA_LEADINGTRIVIA = "XSharp:LeadingTrivia"; // string
        public static string USERDATA_ENDINGTRIVIA = "XSharp:EndingTrivia"; // string
        public static string USERDATA_FROMDESIGNER = "XSharp:FromDesigner";// Logical value
        public static string USERDATA_MODIFIERS = "XSharp:Modifiers"; // string
        public static string USERDATA_ATTRIBUTES = "XSharp:Attributes"; // MemberAttributes
        public static string USERDATA_NOHEADER = "XSharp:NoHeader"; // Logical
        public static string USERDATA_WASWRITTEN = "XSharp:Waswritten"; // Logical value
        public static string USERDATA_GLOBALS = "XSharp:Globals"; // Logical value


        public static void CopyUserData(this CodeObject source, CodeObject target)
        {
            foreach (DictionaryEntry item in source.UserData)
            {
                target.UserData[item.Key] = item.Value;
            }

        }

        private static bool memberEquals(CodeObject oldmember, CodeObject newmember)
        {
            if (oldmember is IXCodeObject)
            {
                var src1 = newmember.GetSourceCode().ToLower().Trim();
                var src2 = oldmember.GetSourceCode().ToLower().Trim();
                if (string.Compare(src1, src2) == 0)
                {
                    return true;
                }
            }
            if (oldmember is CodeMemberField oldfld &&
                newmember is CodeMemberField newfld &&
                oldfld.Name == newfld.Name)
            {
                return true;
            }
            if (oldmember is CodeMemberMethod oldmeth && newmember is CodeMemberMethod newmeth)
            {
                if (oldmeth.Name == newmeth.Name && oldmeth.ReturnType.BaseType == newmeth.ReturnType.BaseType)
                {
                    var oldPars = oldmeth.Parameters;
                    var newPars = newmeth.Parameters;
                    if (oldPars.Count == newPars.Count)
                    {
                        bool parsEqual = true;
                        for (int i = 0; i < oldPars.Count; i++)
                        {
                            var oldPar = oldPars[i];
                            var newPar = newPars[i];
                            if (oldPar.Name != newPar.Name)
                            {
                                parsEqual = false;
                            }
                            else
                            {
                                if (oldPar.Type.BaseType != newPar.Type.BaseType)
                                {
                                    parsEqual = false;
                                }
                            }
                            if (!parsEqual)
                                break;
                        }
                        if (parsEqual)
                        {
                            return true;
                        }
                    }
                }
            }
            if (oldmember is CodeMemberEvent oldevt &&
                newmember is CodeMemberEvent newevt &&
                oldevt.Name == newevt.Name)
            {
                return true;
            }
            if (oldmember is CodeMemberProperty oldprop &&
                newmember is CodeMemberProperty newprop &&
                oldprop.Name == newprop.Name)
            {
                return true;
            }
            return false;
        }

        public static void UpdateClassMemberUserData(this CodeTypeDeclaration source, CodeTypeDeclaration target)
        {
            foreach (CodeObject newmember in source.Members)
            {
                foreach (CodeObject oldmember in target.Members)
                {
                    if (memberEquals(oldmember, newmember))
                    {
                        newmember.CopyUserData(oldmember);
                        break;
                    }
                }
            }

        }

        public static string GetString(this CodeObject e, string id)
        {
            if (e.UserData.Contains(id))
            {
                return (string)e.UserData[id];
            }
            return "";
        }
        public static bool GetBool(this CodeObject e, string id)
        {
            if (e.UserData.Contains(id))
            {
                return (bool)e.UserData[id];
            }
            return false;
        }

        public static bool HasLeadingTrivia(this CodeObject e)
        {
            return e.UserData.Contains(USERDATA_LEADINGTRIVIA);
        }
        public static string GetLeadingTrivia(this CodeObject e)
        {
            return e.GetString(USERDATA_LEADINGTRIVIA);
        }
        public static void SetLeadingTrivia(this CodeObject e, string trivia)
        {
            e.UserData[USERDATA_LEADINGTRIVIA] = trivia;
        }
        public static bool HasEndingTrivia(this CodeObject e)
        {
            return e.UserData.Contains(USERDATA_ENDINGTRIVIA);
        }
        public static string GetEndingTrivia(this CodeObject e)
        {
            return e.GetString(USERDATA_ENDINGTRIVIA);
        }
        public static void SetEndingTrivia(this CodeObject e, string trivia)
        {
            e.UserData[USERDATA_ENDINGTRIVIA] = trivia;
        }

        public static bool HasSourceCode(this CodeObject e)
        {
            return e.UserData.Contains(USERDATA_SOURCECODE);
        }
        public static void SetSourceCode(this CodeObject e, string source)
        {
            e.UserData[USERDATA_SOURCECODE] = source;
        }
        public static string GetSourceCode(this CodeObject e)
        {
            return e.GetString(USERDATA_SOURCECODE);
        }
        public static bool HasFromDesigner(this CodeObject o)
        {
            return o.UserData.Contains(USERDATA_FROMDESIGNER);
        }
        public static bool SourceEquals(this CodeObject o, CodeObject other)
        {
            if (o.HasSourceCode() && other.HasSourceCode())
            {
                var src1 = o.GetSourceCode().Trim().ToLower();
                var src2 = other.GetSourceCode().Trim().ToLower();
                return String.Compare(src1, src2) == 0;
            }
            return false;
        }
        public static bool GetFromDesigner(this CodeObject o)
        {
            if (o.UserData.Contains(USERDATA_FROMDESIGNER))
                return (bool)o.UserData[USERDATA_FROMDESIGNER];
            return false;
        }
        public static void SetNoHeader(this CodeObject o)
        {
            o.UserData[USERDATA_NOHEADER] = true;
        }
        public static bool GetNoHeader(this CodeObject o)
        {
            return o.GetBool(USERDATA_NOHEADER);
        }

        public static void SetGlobals(this CodeObject o, object globals)
        {
            o.UserData[USERDATA_GLOBALS] = globals;
        }
        public static object GetGlobals(this CodeObject o)
        {
            if (o.UserData.Contains(USERDATA_GLOBALS))
                return o.UserData[USERDATA_GLOBALS];
            return null;
        }

        public static void SetFromDesigner(this CodeObject o, bool set)
        {
            o.UserData[USERDATA_FROMDESIGNER] = set;
        }
        public static bool WasWritten(this CodeObject o)
        {
            return o.GetBool(USERDATA_WASWRITTEN);
        }
        public static void SetWritten(this CodeObject o, bool value)
        {
            o.UserData[USERDATA_WASWRITTEN] = value;
        }

        public static bool HasModifiers(this CodeObject o)
        {
            return o.UserData.Contains(USERDATA_MODIFIERS) &&
               o.UserData.Contains(USERDATA_ATTRIBUTES);
        }
        public static MemberAttributes GetMemberAttributes(this CodeObject o)
        {
            if (o.HasModifiers())
            {
                return (MemberAttributes)o.UserData[USERDATA_ATTRIBUTES];
            }
            return (MemberAttributes)0;
        }
        public static string GetModifiers(this CodeObject o)
        {
            if (o.HasModifiers())
            {
                return (string)o.UserData[USERDATA_MODIFIERS];
            }
            return "";
        }
        public static void SetModifiers(this CodeObject o, MemberAttributes a, string modifiers)
        {
            o.UserData[USERDATA_MODIFIERS] = modifiers;
            o.UserData[USERDATA_ATTRIBUTES] = a;
        }

        public static CodeTypeDeclaration GetFirstClass(this CodeCompileUnit ccu)
        {
            foreach (CodeNamespace nameSpace in ccu.Namespaces)
            {
                foreach (CodeTypeDeclaration typeElement in nameSpace.Types)
                {
                    if (typeElement.IsClass)
                        return typeElement;
                }
            }
            return null;

        }

    }
}
