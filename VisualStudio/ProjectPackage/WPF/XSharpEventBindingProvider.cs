//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Drawing;
using System.Globalization;
using System.Text.RegularExpressions;

using Microsoft.VisualStudio;
using Microsoft.VisualStudio.Designer.Interfaces;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.Design.Serialization;
using Microsoft.VisualStudio.Shell.Design.Serialization.CodeDom;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using Microsoft.Windows.Design.Host;

using XSharp.Project;
using Microsoft.VisualStudio.Project;

namespace XSharp.Project.WPF
{
 
    public class XSharpEventBindingProvider : EventBindingProvider
    {
        private IVsProject3 _project;
        private FileNode _xsFile;
        private CodeDomDocDataAdapter _cdda = null;

        internal XSharpEventBindingProvider(FileNode xsFile)
        {
            _xsFile = xsFile;
            _project = xsFile.ProjectMgr;
        }

        public override bool AddEventHandler(EventDescription eventDescription, string objectName, string methodName)
        {
            // we return false here which causes the event handler to always be wired up via XAML instead of via code.
            return false;
        }

        public override bool AllowClassNameForMethodName()
        {
            return true;
        }

        public override bool CreateMethod(EventDescription eventDescription, string methodName, string initialStatements)
        {
            CodeMemberMethod method = new CodeMemberMethod();

            method.Name = methodName;

            foreach (EventParameter param in eventDescription.Parameters)
            {
                method.Parameters.Add(new CodeParameterDeclarationExpression(param.TypeName, param.Name));
            }

            //Finally, add the new method to the class

            CodeDomDocDataAdapter adapter = GetDocDataAdapterForXSharpFile();

            adapter.TypeDeclaration.Members.Add(method);
            adapter.Generate();

            return true;
        }

        public override string CreateUniqueMethodName(string objectName, EventDescription eventDescription)
        {
            string originalMethodName = string.Format(CultureInfo.InvariantCulture, "{0}_{1}", objectName, eventDescription.Name);
            string methodName = originalMethodName;

            List<CodeTypeMember> methods = GetHandlersFromActiveFile(string.Format(CultureInfo.InvariantCulture, "{0}_{1}", objectName, eventDescription.Name));

            while (methods.Count > 0)
            {
                //Try to append a _# at the end until we find an unused method name
                Match match = Regex.Match(methodName, @"_\d+$");

                if (!match.Success)
                {
                    methodName = originalMethodName + "_1";
                }
                else
                {
                    int nextValue = Int32.Parse(match.Value.Substring(1)) + 1;
                    methodName = string.Format(CultureInfo.InvariantCulture, "{0}_{1}", originalMethodName, nextValue);
                }

                methods = GetHandlersFromActiveFile(methodName);
            }
            return methodName;
        }

        public override IEnumerable<string> GetCompatibleMethods(EventDescription eventDescription)
        {
            List<string> methodHandlers = new List<string>();
            // How many parameters
            List<String> pName = new List<string>();
            foreach (EventParameter eParam in eventDescription.Parameters)
            {
                pName.Add(RetrieveFullTypeName(eParam.TypeName));
            }
            // Return Type
            String RetType = RetrieveFullTypeName(eventDescription.ReturnType);
            //
            CodeTypeDeclaration ctd = GetCodeDomForXSharpFile();
            //
            foreach (CodeTypeMember member in ctd.Members)
            {
                if (member is CodeMemberMethod)
                {
                    CodeMemberMethod method = (CodeMemberMethod)member;
                    if ((method.Parameters.Count != pName.Count) || (method.ReturnType.BaseType != RetType))
                        continue;
                    bool allCompatible = true;

                    //compare each parameter
                    for (int i = 0; i < pName.Count; i++)
                    {
                        if (pName[i] != method.Parameters[i].Type.BaseType)
                        {
                            allCompatible = false;
                            break;
                        }
                    }

                    if (allCompatible)
                        yield return method.Name;
                }
            }
        }

        private List<CodeTypeMember> GetHandlersFromActiveFile(string methodName)
        {
            List<CodeTypeMember> methods = new List<CodeTypeMember>();

            //We expect that prg files that contain the event wiring for XAML files contain a namespace
            //and a class.
            foreach (CodeTypeMember member in GetCodeDomForXSharpFile().Members)
            {
                //We just match on the element name here (e.g. button1_Click), not on parameters
                if (member.Name == methodName)
                    methods.Add(member);
            }

            return methods;
        }

        private string RetrieveFullTypeName(string typeName)
        {
            string expName = typeName;
            //
            if (typeName.IndexOf('`') > -1)
            {
                // 
                String genParam = typeName.Substring(typeName.IndexOf('`') + 1, 1);
                //
                int genericArgCount = 0;
                int.TryParse(genParam, out genericArgCount);
                // remove the '`' 
                String genTypeName = typeName.Substring(0, typeName.IndexOf('`'));
                expName = genTypeName;
                if (genericArgCount > 0)
                {
                    //
                    String localName = typeName.Substring(typeName.IndexOf('`') + 2);
                    if (localName.Substring(0, 1) == "[")
                    {
                        localName = localName.Replace("[", "");
                        localName = localName.Replace("]", "");
                    }
                    // NestedType ?
                    localName = RetrieveFullTypeName(localName);
                    expName = expName + "<" + localName + ">";
                }
            }
            return expName;
        }

        public override IEnumerable<string> GetMethodHandlers(EventDescription eventDescription, string objectName)
        {
            List<string> methodHandlers = new List<string>();

            foreach (CodeTypeMember member in GetCodeDomForXSharpFile().Members)
            {
                if (member is CodeConstructor)
                {
                    CodeConstructor constructor = (CodeConstructor)member;

                    foreach (CodeStatement statement in constructor.Statements)
                    {
                        if (statement is CodeAttachEventStatement)
                        {
                            CodeAttachEventStatement codeAttach = (CodeAttachEventStatement)statement;

                            if (codeAttach.Event.EventName != eventDescription.Name)
                            {
                                //This is a code attach, but not for the event that the designer is looking for.
                                //Go to the next one.
                                continue;
                            }

                            if (codeAttach.Event.TargetObject is CodeMethodInvokeExpression)
                            {
                                CodeMethodInvokeExpression findLogNode = (CodeMethodInvokeExpression)codeAttach.Event.TargetObject;

                                if (findLogNode.Parameters.Count >= 2 &&
                                    findLogNode.Parameters[1] is CodePrimitiveExpression)
                                {
                                    string targetObjectName = ((CodePrimitiveExpression)findLogNode.Parameters[1]).Value.ToString().Trim('"');

                                    if (targetObjectName.Equals(objectName, StringComparison.Ordinal) &&
                                        codeAttach.Listener is CodeDelegateCreateExpression)
                                    {
                                        methodHandlers.Add(((CodeDelegateCreateExpression)codeAttach.Listener).MethodName);
                                    }
                                }
                            }
                        }
                    }
                }
            }

            return methodHandlers;
        }

        public override bool IsExistingMethodName(EventDescription eventDescription, string methodName)
        {
            List<CodeTypeMember> elements = GetHandlersFromActiveFile(methodName);
            return elements.Count != 0;
        }

        public override bool RemoveEventHandler(EventDescription eventDescription, string objectName, string methodName)
        {
            throw new NotImplementedException();
        }

        public override bool RemoveMethod(EventDescription eventDescription, string methodName)
        {
            throw new NotImplementedException();
        }

        public override void SetClassName(string className)
        {
            return;
        }

        public override bool ShowMethod(EventDescription eventDescription, string methodName)
        {
            CodeDomDocDataAdapter adapter = GetDocDataAdapterForXSharpFile();
            List<CodeTypeMember> methodsToShow = GetHandlersFromActiveFile(methodName);

            if (methodsToShow == null || methodsToShow.Count < 1)
                return false;

            Point point = new Point();

            if (methodsToShow[0] != null)
            {
                //We can't navigate to every method, so just take the first one in the list.
                object pt = methodsToShow[0].UserData[typeof(Point)];

                if (pt != null)
                {
                    point = (Point)pt;
                }
            }

            //Get IVsTextManager to navigate to the code
            IVsTextManager mgr = Microsoft.VisualStudio.Shell.Package.GetGlobalService(typeof(VsTextManagerClass)) as IVsTextManager;
            Guid logViewCode = VSConstants.LOGVIEWID_Code;

            return ErrorHandler.Succeeded(mgr.NavigateToLineAndColumn(adapter.DocData.Buffer, ref logViewCode, point.Y - 1, point.X, point.Y - 1, point.X));
        }

        public override void ValidateMethodName(EventDescription eventDescription, string methodName)
        {
            return;
        }

        private static EnvDTE.DTE dte;
        private static EnvDTE.DTE DTE
        {
            get
            {
                if (dte == null)
                {
                    ThreadHelper.JoinableTaskFactory.Run(async delegate
                    {
                        await ThreadHelper.JoinableTaskFactory.SwitchToMainThreadAsync();
                    dte = Microsoft.VisualStudio.Shell.Package.GetGlobalService(typeof(EnvDTE.DTE)) as EnvDTE.DTE;
                    });
                }
                return dte;
            }
        }

        public override string CodeProviderLanguage
        {
            get
            {
                return "XSharp";
            }
        }

        /// <summary>
        /// This method will get the CodeDomDocDataAdapter corresponding to the active XAML file in
        /// the designer.
        /// </summary>
        /// <returns>The CodeDomDocDataAdapter for the .prg file that corresponds to the active xaml file</returns>
        CodeDomDocDataAdapter GetDocDataAdapterForXSharpFile()
        {
            if (_cdda == null)
            {
                var codeDom = (IVSMDCodeDomProvider)(new ServiceProvider(_xsFile.OleServiceProvider, true)).GetService(typeof(SVSMDCodeDomProvider));
                var data = new DocData(((XSharpProjectNode)_project).ProjectMgr.Site, _xsFile.Url);

                _cdda = new CodeDomDocDataAdapter((_project as XSharpProjectNode).ProjectMgr.Site, data);
            }
            return _cdda;
        }

        /// <summary>
        /// This method will get the CodeTypeDeclaration corresponding to the active XAML file in
        /// the designer.
        /// </summary>
        /// <returns>The CodeTypeDeclaration for the .prg file that corresponds to the active xaml file</returns>
        CodeTypeDeclaration GetCodeDomForXSharpFile()
        {
            CodeDomDocDataAdapter cdda = GetDocDataAdapterForXSharpFile();
            // That will call the Parse method in VSXSharpCodeDomProvider class
            CodeTypeDeclaration ctd = cdda.TypeDeclaration;
            //
            CodeCompileUnit ccu = cdda.CompileUnit;
            //
            return ctd;
        }



        public override bool RemoveHandlesForName(string elementName)
        {
            return true; 
        }

        public override void AppendStatements(EventDescription eventDescription, string methodName, string statements, int relativePosition)
        {
            //??
        }
    }
}
