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
        METHOD GetRecno(nPos AS Int32) AS Int32
        METHOD GetKey(nPos AS Int32) AS BYTE[]
        PROPERTY NumKeys  AS WORD GET
    END INTERFACE
END NAMESPACE
