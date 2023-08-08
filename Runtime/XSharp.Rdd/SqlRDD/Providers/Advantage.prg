//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.Collections.Generic
USING System.Text
USING XSharp.RDD.SqlRDD
USING System.Data.Common
using System.Reflection

BEGIN NAMESPACE XSharp.RDD.SqlRDD.Providers

/// <summary>
/// The ODBC class.
/// </summary>
CLASS Advantage INHERIT SqlDbProvider
    override property DllName as string => "Advantage.Data.Provider.Dll"
    override property TypeName as string => "Advantage.Data.Provider.AdsFactory"
    CONSTRUCTOR()
        SUPER("Advantage")
        RETURN
    private static aFuncs := NULL as Dictionary<String, String>
    OVERRIDE METHOD GetFunctions() AS Dictionary<String, String>
        if aFuncs == NULL
            aFuncs := Dictionary<String, String>{StringComparer.OrdinalIgnoreCase}
        endif
        return aFuncs

END CLASS
END NAMESPACE // XSharp.RDD.SqlRDD.SupportClasses
