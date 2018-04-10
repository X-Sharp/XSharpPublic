//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
using EnvDTE
using LanguageService.CodeAnalysis
using LanguageService.CodeAnalysis.XSharp
using System.Collections.Concurrent
using System.Collections.Immutable
using EnvDTE80
using Microsoft.VisualStudio
using Microsoft.VisualStudio.Shell.Interop
using System.Diagnostics

begin namespace XSharpModel
	class OrphanedFilesProject implements IXSharpProject
	#region properties
		property IntermediateOutputPath as string get ""
		property IsVsBuilding as logic get false
		property OutputFile as string get ""
		property ParseOptions as XSharpParseOptions get XSharpParseOptions.Default
		property PrefixClassesWithDefaultNamespace as logic get false
		property Project as XProject auto
		property RootNameSpace as string get "" 
		property Url as string get "" 
		property DisableRegions as logic get false
		property DisableLexing as logic get false
		property DisableParsing as logic get false
		property KeywordsUppercase as logic get TRUE
	#endregion
		
		
		virtual method AddFileNode(strFileName as string) as void
			return
		
		virtual method AddIntellisenseError(file as string, line as long, column as long, Length as long, errCode as string, message as string, sev as DiagnosticSeverity) as void
			return
		
		
		virtual method ClearIntellisenseErrors(file as string) as void
			return
		
		
		virtual method DeleteFileNode(strFileName as string) as void
			return
		
		
		virtual method DocumentGetText(file as string, isOpen ref logic) as string
			//
			isOpen := false
			return ""
		
		virtual method DocumentInsertLine(fileName as string, line as long, text as string) as logic
			//
			return false
		
		virtual method DocumentSetText(fileName as string, text as string) as logic
			//
			return false
		
		virtual method FindProject(sProject as string) as Project
			//
			return null
		
		virtual method GetIntellisenseErrorPos(fileName as string) as System.Collections.Generic.List<IXErrorPosition>
			return List<IXErrorPosition>{}
		
		virtual method HasFileNode(strFileName as string) as logic
			return true
		
		virtual method IsDocumentOpen(file as string) as logic
			return true
		
		virtual method OpenElement(file as string, line as long, column as long) as void
			return
		
		
		virtual method SetStatusBarAnimation(onoff as logic, id as short) as void
			return
		
		
		virtual method SetStatusBarText(message as string) as void
			return
		
		
		virtual method ShowIntellisenseErrors() as void
			return
		
		
		
		
		
	end class
	
end namespace 

