// CdxInterfaces.prg
// Created by    : robert
// Creation Date : 11/12/2018 5:20:44 PM
// Created for   : 
// WorkStation   : ARTEMIS


USING System
USING System.Collections.Generic
USING System.Text
BEGIN NAMESPACE XSharp.RDD.CDX

    INTERNAL INTERFACE ICdxKeyValue
        METHOD GetRecno(nPos as Int32) as Int32
        Method GetKey(nPos as Int32) as BYTE[]
    END INTERFACE
END NAMESPACE
