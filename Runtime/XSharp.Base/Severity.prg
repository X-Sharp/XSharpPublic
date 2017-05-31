//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp
Enum Severity
    Member WHOCARES     := 0
    Member WARNING      := 1
    Member ERROR        := 2
    Member CATASTROPHIC := 3
END Enum
END NAMESPACE
#region Severity Defines
define ES_WHOCARES     := Severity.WHOCARES    
define ES_WARNING      := Severity.WARNING     
define ES_ERROR        := Severity.ERROR       
define ES_CATASTROPHIC := Severity.CATASTROPHIC

#endregion