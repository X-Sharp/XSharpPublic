// TestDBF.prg
// Created by    : fabri
// Creation Date : 4/24/2018 5:21:57 PM
// Created for   : 
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING Xunit
USING XSharp.RDD

BEGIN NAMESPACE XSharp.RDD.Tests
    
    /// <summary>
    /// The TestDBF class.
    /// </summary>
    CLASS TestDBF
        
        [Fact, Trait("Dbf", "Open")];
            METHOD OpenDBF() AS VOID
            VAR dbInfo := DbOpenInfo{ "dbase_03.DBF", "dbase_03", 1, FALSE, FALSE }
            //
            VAR myDBF := DBF{}
            myDBF:Open( dbInfo )
            //

            RETURN
        
    END CLASS
END NAMESPACE // XSharp.RDD.Tests