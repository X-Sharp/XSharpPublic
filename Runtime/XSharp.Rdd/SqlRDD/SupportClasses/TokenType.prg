// TokenType.prg
// Created by    : robert
// Creation Date : 2/15/2023 6:39:18 PM
// Created for   :
// WorkStation   : NYX


USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.RDD.SqlRDD

	/// <summary>
    /// The TokenType class.
    /// </summary>
	ENUM TokenType
        MEMBER None     := 0
        MEMBER Token
        MEMBER BoExpr
        MEMBER EoExpr
        MEMBER BoFunc
        MEMBER EoFunc
        MEMBER Delimiter
        MEMBER Operator
        MEMBER Logic
        MEMBER String
        MEMBER Literal
        MEMBER Blank
	END ENUM
END NAMESPACE // XSharp.RDD.SqlRDD
