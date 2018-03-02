//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


begin namespace XSharp
/// <Summary>Enum that matches the Visual Objects ES_* defines</Summary>
enum Severity
	member WHOCARES     := 0
	member WARNING      := 1
	member ERROR        := 2
	member CATASTROPHIC := 3
end enum
end namespace


#region Severity Defines
define ES_WHOCARES     := Severity.WHOCARES    
define ES_WARNING      := Severity.WARNING     
define ES_ERROR        := Severity.ERROR       
define ES_CATASTROPHIC := Severity.CATASTROPHIC

#endregion