
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.VisualStudio.Language.Intellisense;
using System.Collections.ObjectModel;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Tagging;
using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Utilities;
using XSharpModel;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using System.Windows.Media;
using LanguageService.SyntaxTree;
using LanguageService.CodeAnalysis.XSharp.SyntaxParser;
using System.Reflection;
using Microsoft.VisualStudio;
using LanguageService.CodeAnalysis.XSharp;

namespace XSharpLanguage
{

    /*
     * 
    partial class XSharpCompletionSource
    {
        /// <summary>
        /// Search for a Property, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Property we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Property (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private CompletionType SearchPropertyTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility)
        {
            if (cType.XType != null)
            {
                XTypeMember element = cType.XType.Members.Find(x =>
                {
                    if ((x.Kind == Kind.Property) || (x.Kind == Kind.Access) || (x.Kind == Kind.Assign))
                    {
                        return (StringEquals(x.Name, currentToken));
                    }
                    return false;
                });
                //
                if ((element != null) && (element.Visibility < minVisibility))
                {
                    element = null;
                }
                //
                if (element == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return SearchPropertyTypeIn(new CompletionType(cType.XType.Parent), currentToken, Modifiers.Public);
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchPropertyTypeIn(new CompletionType(cType.XType.ParentName, cType.XType.FileUsings), currentToken, Modifiers.Public);
                    }
                }
                else
                {
                    cType = new CompletionType((XTypeMember)element);
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetProperties(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetProperties(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                }
                //
                Type declType = null;
                foreach (var member in members)
                {
                    if (StringEquals(member.Name, currentToken))
                    {
                        PropertyInfo prop = member as PropertyInfo;
                        declType = prop.PropertyType;
                        break;
                    }
                }
                if (declType == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        return SearchPropertyTypeIn(new CompletionType(cType.SType.BaseType), currentToken, Modifiers.Public);
                    }
                    // Not needed: no properties inside object type
                    //else if (cType.SType.IsInterface)
                    //{
                    //    return SearchPropertyTypeIn(new CompletionType(typeof(object)), currentToken, Modifiers.Public);
                    //}

                }
                else
                {
                    return new CompletionType(declType);
                }
            }
            // Sorry, not found
            return new CompletionType();
        }

        /// <summary>
        /// Search for a Field, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Field we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType of the Field (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private CompletionType SearchFieldTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility)
        {
            if (cType.XType != null)
            {
                XTypeMember element = cType.XType.Members.Find(x =>
                {
                    if ((x.Kind == Kind.ClassVar))
                    {
                        return StringEquals(x.Name, currentToken);
                    }
                    return false;
                });
                //
                if ((element != null) && (element.Visibility < minVisibility))
                {
                    element = null;
                }
                //
                if (element == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return SearchFieldTypeIn(new CompletionType(cType.XType.Parent), currentToken, Modifiers.Public);
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchFieldTypeIn(new CompletionType(cType.XType.ParentName, cType.XType.FileUsings), currentToken, Modifiers.Public);
                    }
                }
                else
                {
                    cType = new CompletionType((XTypeMember)element);
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetFields(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetFields(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                }
                //
                Type declType = null;
                foreach (var member in members)
                {
                    if (StringEquals(member.Name, currentToken))
                    {
                        FieldInfo field = member as FieldInfo;
                        declType = field.FieldType;
                        break;
                    }
                }
                if (declType == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        return SearchFieldTypeIn(new CompletionType(cType.SType.BaseType), currentToken, Modifiers.Public);
                    }
                    // not needed: no fields inside object type
                    //else if (cType.SType.IsInterface)
                    //{
                    //    return SearchFieldTypeIn(new CompletionType(typeof(object)), currentToken, Modifiers.Public);
                    //}

                }
                else
                {
                    return new CompletionType(declType);
                }
            }
            // Sorry, not found
            return new CompletionType();
        }

        /// <summary>
        /// Search for a Method, in a CompletionType, based on the Visibility.
        /// A Completion can have a XType (XSharp parsed type) or a SType (A System type or a Type found inside a library Reference)
        /// </summary>
        /// <param name="cType">The CompletionType to look into</param>
        /// <param name="currentToken">The Method we are searching</param>
        /// <param name="minVisibility"></param>
        /// <returns>The CompletionType that the Method will return (If found).
        /// If not found, the CompletionType.IsInitialized is false
        /// </returns>
        private CompletionType SearchMethodTypeIn(CompletionType cType, string currentToken, Modifiers minVisibility)
        {
            if (cType.XType != null)
            {
                // 
                XTypeMember xMethod = cType.XType.Members.Find(x =>
                {
                    if ((x.Kind == Kind.Method))
                    {
                        return (StringEquals(x.Name, currentToken));
                    }
                    return false;
                });
                //if (elt.IsStatic)
                //    continue;
                if ((xMethod != null) && (xMethod.Visibility < minVisibility))
                {
                    xMethod = null;
                }
                //
                if (xMethod == null)
                {
                    // Hummm, we should look inside the Owner
                    cType.XType.ForceComplete();
                    if (cType.XType.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return SearchMethodTypeIn(new CompletionType(cType.XType.Parent), currentToken, Modifiers.Public);
                    }
                    else if (cType.XType.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return SearchMethodTypeIn(new CompletionType(cType.XType.ParentName, cType.XType.FileUsings), currentToken, Modifiers.Public);
                    }
                }
                else
                {
                    if (xMethod.Parent != null)
                    {
                        // Parent is a XElement, so one of our Types
                        return new CompletionType(xMethod.Parent);
                    }
                    else if (xMethod.ParentName != null)
                    {
                        // Parent has just a Name, so one of the System Types
                        return new CompletionType(xMethod.ParentName, xMethod.FileUsings);
                    }
                }
            }
            else if (cType.SType != null)
            {
                MemberInfo[] members;
                //
                if (minVisibility < Modifiers.Public)
                {
                    // Get Public, Internal, Protected & Private Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetMethods(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static);
                }
                else
                {
                    //  Get Public Members, we also get Instance vars, Static members...all that WITHOUT inheritance
                    members = cType.SType.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static);
                }
                //
                Type declType = null;
                foreach (var member in members)
                {
                    if (member.MemberType == MemberTypes.Method)
                    {
                        if (StringEquals(member.Name, currentToken))
                        {
                            MethodInfo method = member as MethodInfo;
                            declType = method.ReturnType;
                            break;
                        }
                    }
                }
                if (declType == null)
                {
                    // In the parent ?
                    if (cType.SType.BaseType != null)
                    {
                        return SearchMethodTypeIn(new CompletionType(cType.SType.BaseType), currentToken, Modifiers.Public);
                    }
                    // Look for methods in the object type
                    if (cType.SType.IsInterface)
                    {
                        return SearchMethodTypeIn(new CompletionType(typeof(object)), currentToken, Modifiers.Public);
                    }
                }
                else
                {
                    return new CompletionType(declType);
                }
            }
            // Sorry, not found
            return new CompletionType();
        }
        
    }
*/
}