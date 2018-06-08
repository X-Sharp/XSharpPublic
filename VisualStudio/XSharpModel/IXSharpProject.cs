//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis;
using LanguageService.CodeAnalysis.XSharp;
using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Shell.TableManager;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace XSharpModel
{
    public interface IXSharpProject
    {
        bool IsVsBuilding { get; }
        string RootNameSpace { get; }
        bool PrefixClassesWithDefaultNamespace { get; }
        string Url { get; }
        string OutputFile { get; }
        void SetStatusBarText(string message);
        void SetStatusBarAnimation(bool onOff, short id);
        void OpenElement(string file, int line, int column);

        void ClearIntellisenseErrors(string file);
        void AddIntellisenseError(string file, int line, int column, int Length, string errCode, string message, DiagnosticSeverity sev);
        void ShowIntellisenseErrors();

        bool IsDocumentOpen(string file);
        string DocumentGetText(string file, ref bool IsOpen);
        bool DocumentInsertLine(string fileName, int line, string text);
        bool DocumentSetText(string fileName, string text);

        List<IXErrorPosition> GetIntellisenseErrorPos(string fileName);
        XSharpParseOptions ParseOptions { get; }
        EnvDTE.Project FindProject(String sProject);
        string IntermediateOutputPath { get; }

        void AddFileNode(string fileName);
        void DeleteFileNode(string fileName);

        bool HasFileNode(string fileName);
    }

    public interface IXErrorPosition
    {
        int Line { get; }
        int Column { get; }
        int Length { get; }
    }

}
