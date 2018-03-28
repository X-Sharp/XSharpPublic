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

BEGIN NAMESPACE XSharpModel
    CLASS OrphanedFilesProject IMPLEMENTS IXSharpProject

        // Methods
        VIRTUAL METHOD AddFileNode(strFileName AS string) AS void


        VIRTUAL METHOD AddIntellisenseError(file AS string, line AS Long, column AS Long, Length AS Long, errCode AS string, message AS string, sev AS DiagnosticSeverity) AS void


        VIRTUAL METHOD ClearIntellisenseErrors(file AS string) AS void


        VIRTUAL METHOD DeleteFileNode(strFileName AS string) AS void


        VIRTUAL METHOD DocumentGetText(file AS string, isOpen REF Logic) AS string
            //
            isOpen := FALSE
            RETURN ""

        VIRTUAL METHOD DocumentInsertLine(fileName AS string, line AS Long, text AS string) AS Logic
            //
            RETURN FALSE

        VIRTUAL METHOD DocumentSetText(fileName AS string, text AS string) AS Logic
            //
            RETURN FALSE

        VIRTUAL METHOD FindProject(sProject AS string) AS Project
            //
            RETURN null

        VIRTUAL METHOD GetIntellisenseErrorPos(fileName AS string) AS System.Collections.Generic.List<IXErrorPosition>
            RETURN List<IXErrorPosition>{}

        VIRTUAL METHOD HasFileNode(strFileName AS string) AS Logic
            RETURN TRUE

        VIRTUAL METHOD IsDocumentOpen(file AS string) AS Logic
            RETURN TRUE

        VIRTUAL METHOD OpenElement(file AS string, line AS Long, column AS Long) AS void


        VIRTUAL METHOD SetStatusBarAnimation(onoff AS Logic, id AS Short) AS void


        VIRTUAL METHOD SetStatusBarText(message AS string) AS void


        VIRTUAL METHOD ShowIntellisenseErrors() AS void



        // Properties
        VIRTUAL PROPERTY IntermediateOutputPath AS string
            GET
                //
                RETURN ""
            END GET
        END PROPERTY

        VIRTUAL PROPERTY IsVsBuilding AS Logic
            GET
                //
                RETURN FALSE
            END GET
        END PROPERTY

        VIRTUAL PROPERTY OutputFile AS string
            GET
                //
                RETURN ""
            END GET
        END PROPERTY
		PROPERTY ParseOptions AS XSharpParseOptions GET XSharpParseOptions.Default
        PROPERTY PrefixClassesWithDefaultNamespace AS Logic GET FALSE

        PROPERTY Project AS XProject AUTO

        PROPERTY RootNameSpace AS string GET "" 
        PROPERTY Url AS string GET "" 


    END CLASS

END NAMESPACE 

