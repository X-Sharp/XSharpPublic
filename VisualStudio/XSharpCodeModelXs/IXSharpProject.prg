//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis
using LanguageService.CodeAnalysis.XSharp
using Microsoft.VisualStudio.Shell
using Microsoft.VisualStudio.Shell.TableManager
using System
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
begin namespace XSharpModel
	interface IXSharpProject
		method AddFileNode(fileName as string) as void
		method AddIntellisenseError(file as string, line as long, column as long, Length as long, errCode as string, message as string, sev as DiagnosticSeverity) as void
		method ClearIntellisenseErrors(file as string) as void
		method DeleteFileNode(fileName as string) as void
		method DocumentGetText(file as string, IsOpen ref logic) as string
		method DocumentInsertLine(fileName as string, line as long, text as string) as logic
		method DocumentSetText(fileName as string, text as string) as logic
		method FindProject(sProject as string) as EnvDTE.Project
		method GetIntellisenseErrorPos(fileName as string) as System.Collections.Generic.List<IXErrorPosition>
		method HasFileNode(fileName as string) as logic
		method IsDocumentOpen(file as string) as logic
		method OpenElement(file as string, line as long, column as long) as void
		method SetStatusBarAnimation(onOff as logic, id as short) as void
		method SetStatusBarText(message as string) as void
		method ShowIntellisenseErrors() as void
		
		// Properties
		property IntermediateOutputPath as string get 
		property IsVsBuilding as logic get 
		property OutputFile as string get 
		property ParseOptions as XSharpParseOptions get 
		property PrefixClassesWithDefaultNamespace as logic get 
		property RootNameSpace as string get 
		property Url as string get 
		
	end interface
	
end namespace 

