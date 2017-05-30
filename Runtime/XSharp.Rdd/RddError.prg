
USING System
USING System.Collections.Generic
USING System.Text

BEGIN NAMESPACE XSharp.Rdd

	CLASS RddError INHERIT Exception
        PROPERTY SubSystem as STRING AUTO
        PROPERTY Gencode as DWORD AUTO
        PROPERTY SubCode as DWORD AUTO
        PROPERTY FuncSym as STRING AUTO
        PROPERTY FileName as STRING AUTO
        PROPERTY Severity as DWORD AUTO
        PROPERTY Description as STRING AUTO
    CONSTRUCTOR()
         RETURN

	END CLASS
END NAMESPACE // XSharp.Rdd