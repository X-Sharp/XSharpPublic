//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

using System
begin namespace XSharpModel
	
	static class XFileTypeHelpers
		// Methods
		static method GetFileType(filename as string) as XFileType
			
			var ext := System.IO.Path.GetExtension(filename):ToLower()
			switch ext
				case ".prg"
				case ".xs"
					return XFileType.SourceCode
				case ".ppo"
					return XFileType.PreprocessorOutput
				case ".vh"
				case ".xh"
					return XFileType.Header
				case ".xsfrm"
				case ".vnfrm"
					return XFileType.VOForm
				case ".xsmnu"
				case ".vnmnu"
					return XFileType.VOMenu
				case ".xsdbs"
				case ".vndbs"
					return XFileType.VODBServer
				case ".xsfs"
				case ".vnfs"
					return XFileType.VOFieldSpec
				case ".xaml"
					return XFileType.XAML
				case ".settings"
					return XFileType.Settings
				case ".resx"
					return XFileType.ManagedResource
				case ".licx"
					return XFileType.License
				case ".rc"
					return XFileType.NativeResource
			end switch
			return XFileType.Unknown
		
		static method IsVOBinary( self type as XFileType) as logic
			switch (type)
				case XFileType.VOMenu
				case XFileType.VODBServer
				case XFileType.VOFieldSpec
				case XFileType.VOForm
				case XFileType.VOIndex
				case XFileType.VOOrder
					return true
			end switch
			return false 
		
		static method OpenInSourceCodeEditor( self type as XFileType) as logic
			switch (type)
				case XFileType.SourceCode
				case XFileType.Header
				case XFileType.NativeResource
				case XFileType.Unknown
					return true
			end switch
			return false
		
		
	end class

end namespace 

