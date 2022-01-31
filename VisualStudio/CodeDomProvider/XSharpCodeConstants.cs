﻿using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharp.CodeDom
{
    public static class XSharpCodeConstants
    {
        public static string USERDATA_SOURCECODE = "XSharp:Sourcecode"; // string
        public static string USERDATA_LEADINGTRIVIA = "XSharp:LeadingTrivia"; // string
        public static string USERDATA_ENDINGTRIVIA = "XSharp:EndingTrivia"; // string
        public static string USERDATA_CODEBEFORE = "XSharp:CodeBefore"; // string
        public static string USERDATA_FROMDESIGNER = "XSharp:FromDesigner";// Logical value
        public static string USERDATA_MODIFIERS = "XSharp:Modifiers"; // string
        public static string USERDATA_ATTRIBUTES = "XSharp:Attributes"; // MemberAttributes
        public static string USERDATA_NOHEADER = "XSharp:NoHeader"; // Logical
        public static string USERDATA_WASWRITTEN = "XSharp:Waswritten"; // Logical value


        public static bool HasLeadingTrivia(this CodeObject e)
        {
            return e.UserData.Contains(USERDATA_LEADINGTRIVIA);
        }
        public static bool HasEndingTrivia(this CodeObject e)
        {
            return e.UserData.Contains(USERDATA_ENDINGTRIVIA);
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
            if (e.HasSourceCode())
            {
                var result = (string)e.UserData[USERDATA_SOURCECODE];
                return result;
            }
            return "";
        }
        public static bool HasFromDesigner(this CodeObject o)
        {
            return o.UserData.Contains(USERDATA_FROMDESIGNER);
        }
        public static bool GetFromDesigner(this CodeObject o)
        {
            return (bool)o.UserData[USERDATA_FROMDESIGNER];
        }
        public static void SetNoHeader(this CodeObject o)
        {
            o.UserData[USERDATA_NOHEADER] = true;
        }
        public static bool GetNoHeader(this CodeObject o)
        {
            if (o.UserData.Contains(USERDATA_NOHEADER))
                return (bool) o.UserData[USERDATA_NOHEADER] ;
            return false;
        }
       
        
        public static void SetFromDesigner(this CodeObject o, bool set)
        {
            o.UserData[USERDATA_FROMDESIGNER] = set;
        }
        
        public static bool WasWritten(this CodeObject o)
        {
            if (o.UserData.Contains(USERDATA_WASWRITTEN))
                return (bool)o.UserData[USERDATA_WASWRITTEN];
            return false;
        }
        public static void SetWritten(this CodeObject o, bool value)
        {
            o.UserData[USERDATA_WASWRITTEN] = value;
        }

        public static bool HasCodeBefore(this CodeObject o)
        {
            return o.UserData.Contains(USERDATA_CODEBEFORE);
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


        public static string GetCodeBefore(this CodeObject o)
        {
            if (o.HasCodeBefore())
                return (string)o.UserData[USERDATA_CODEBEFORE];
            return "";
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
