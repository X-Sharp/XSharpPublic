//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


BEGIN NAMESPACE XSharp
/// <Summary>Enum that matches the Visual Objects ES_* defines</Summary>
ENUM Severity
	MEMBER WHOCARES     := 0
	MEMBER WARNING      := 1
	MEMBER ERROR        := 2
	MEMBER CATASTROPHIC := 3
END ENUM
END NAMESPACE
#region Severity Defines
DEFINE ES_WHOCARES     := Severity.WHOCARES    
DEFINE ES_WARNING      := Severity.WARNING     
DEFINE ES_ERROR        := Severity.ERROR       
DEFINE ES_CATASTROPHIC := Severity.CATASTROPHIC
	
#endregion