// TokenType.prg
// Created by    : robert
// Creation Date : 2/15/2023 6:39:18 PM
// Created for   :
// WorkStation   : NYX


using System
using System.Collections.Generic
using System.Text

begin namespace XSharp.RDD.SqlRDD

/// <exclude />
internal enum TokenType
    member None     := 0
    member Token
    member BoExpr
    member EoExpr
    member BoFunc
    member EoFunc
    member Delimiter
    member Operator
    member Logic
    member String
    member Literal
    member Blank
end enum
end namespace // XSharp.RDD.SqlRDD
