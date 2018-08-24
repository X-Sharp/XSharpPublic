//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
BEGIN NAMESPACE XSharpModel

	STATIC CLASS XFileTypeHelpers
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
			END SWITCH
			RETURN XFileType.Unknown

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
					RETURN TRUE
			END SWITCH
			RETURN FALSE


	END CLASS

END NAMESPACE

