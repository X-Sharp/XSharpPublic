//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
using XSharp.Settings
BEGIN NAMESPACE XSharpModel

STATIC CLASS XFileTypeHelpers

    STATIC METHOD GetXSharpTypeName( SELF sysType AS Mono.Cecil.TypeReference) AS STRING
        var fullName := sysType:FullName
        IF (fullName == NULL)
            fullName := sysType:Name
        ENDIF
        local suffix := TypeExtensions.GetTypeSuffix(ref fullName) as string
        fullName := fullName:GetXSharpTypeName()
        // Maybe it's a Raw format ?
        LOCAL genMarker := fullName:IndexOf('`') AS INT
        IF (genMarker > -1)
            // First extract the type
            LOCAL genTypeName := fullName:Substring(0, genMarker) AS STRING
            VAR genericString := "<"
            VAR GenericParameters := sysType:GenericParameters
            LOCAL first := TRUE AS LOGIC
            FOREACH VAR genArg IN GenericParameters
                IF first
                    genericString += genArg:Name
                    first := FALSE
                ELSE
                    genericString += "," + genArg:Name
                ENDIF
            NEXT
            //
            genericString += ">"
            fullName := genTypeName + genericString
        ENDIF
        if suffix:Length > 0
            fullName += suffix
        endif
        RETURN fullName


    // Methods
    STATIC METHOD GetFileType(filename AS STRING) AS XFileType

        VAR ext := System.IO.Path.GetExtension(filename):ToLower()
        SWITCH ext
        CASE ".prg"
        CASE ".xs"
            RETURN XFileType.SourceCode
        CASE ".ppo"
            RETURN XFileType.PreprocessorOutput
        CASE ".vh"
        CASE ".xh"
        CASE ".ch"
            RETURN XFileType.Header
        CASE ".xsfrm"
        CASE ".vnfrm"
            RETURN XFileType.VOForm
        CASE ".xsmnu"
        CASE ".vnmnu"
            RETURN XFileType.VOMenu
        CASE ".xsdbs"
        CASE ".vndbs"
            RETURN XFileType.VODBServer
        CASE ".xsfs"
        CASE ".vnfs"
            RETURN XFileType.VOFieldSpec
        CASE ".xaml"
            RETURN XFileType.XAML
        CASE ".settings"
            RETURN XFileType.Settings
        CASE ".resx"
            RETURN XFileType.ManagedResource
        CASE ".licx"
            RETURN XFileType.License
        CASE ".rc"
            RETURN XFileType.NativeResource
        CASE ".bmp"
        CASE ".ico"
        CASE ".jpg"
        CASE ".jpeg"
        CASE ".gif"
        CASE ".png"
            RETURN XFileType.Resource
        CASE ".tpl"
        CASE ".inf"
            RETURN XFileType.Template
        CASE ".tt"
            RETURN XFileType.TextTemplate
        CASE ".config"
            RETURN XFileType.Config


        END SWITCH
        RETURN XFileType.Other

    STATIC METHOD IsVOBinary( SELF type AS XFileType) AS LOGIC
        SWITCH (type)
        CASE XFileType.VOMenu
        CASE XFileType.VODBServer
        CASE XFileType.VOFieldSpec
        CASE XFileType.VOForm
        CASE XFileType.VOIndex
        CASE XFileType.VOOrder
            RETURN TRUE
        END SWITCH
        RETURN FALSE

    STATIC METHOD OpenInSourceCodeEditor( SELF type AS XFileType) AS LOGIC
        SWITCH (type)
        CASE XFileType.SourceCode
        CASE XFileType.Header
        CASE XFileType.NativeResource
        CASE XFileType.Unknown
        CASE XFileType.Template
        CASE XFileType.Config
        CASE XFileType.Other
            RETURN TRUE
        END SWITCH
        RETURN FALSE


END CLASS

END NAMESPACE

