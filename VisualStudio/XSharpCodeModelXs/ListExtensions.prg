//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System.Linq
using System.Collections.Generic
using System
using System.Collections.Immutable
begin namespace XSharpModel
	
	static class ListExtensions
		static method AddUnique( self list as List<string>, item as string) as void
			if !list:Contains(item, System.StringComparer.OrdinalIgnoreCase)
				list:Add(item)
			endif
		
		static method Expanded( self source as IEnumerable<string>) as IReadOnlyList<string>
			local list as List<string>
			local item as string
			list := List<string>{}
			list:AddRange(source)
			foreach str as string in source
				item := str
				while (item:Contains("."))
					item := item:Substring(0, item:LastIndexOf("."))
					if (! list:Contains(item))
						list:Add(item)
					endif
				enddo
			next
			return List:ToImmutableList() 
		
		
	end class
	
end namespace 

