//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
using XUnit



BEGIN NAMESPACE XSharp.Core.Tests

	CLASS ErrorTests

		[Trait("Category", "Error")];
		[Fact];
		METHOD Test() AS VOID 
			local dw as dword
			Assert.NotEmpty(ErrString(Gencode.EG_ARG))
			Assert.NotEmpty(ErrString(EG_ARG))
			dw := EG_ARG
			Assert.NotEmpty(ErrString(dw))
			
		return

	END CLASS
END NAMESPACE // XSharp.Runtime.Tests