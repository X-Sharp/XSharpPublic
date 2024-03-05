
#command COPY [STRUCTURE] [EXTENDED] [TO <(file)>]   ;
    => DbCopyXStructFox( <(file)>)


#command COPY [STRUCTURE] [EXTENDED] [TO <(file)>]  ;
    DATABASE <dbname> [NAME <longtablename>] ;
    => DbCopyXStructFox( <(file)>) ;; DbAdd(<(dbname)>,  <(file)>, <(longtablename)>)


#command COPY [STRUCTURE] [TO <(file)>] [__DBFIELDLIST__ <fields>]  ;
    [WITH <cdx:CDX, PRODUCTION>] ;
    => DbCopyStructFox( <(file)>, <fields> , <.cdx.>)

#command COPY [STRUCTURE] [TO <(file)>] [__DBFIELDLIST__ <fields>]  ;
    [WITH <cdx:CDX, PRODUCTION>] ;
    DATABASE <dbname> [NAME <longtablename>] ;
    => DbCopyStructFox( <(file)>, <fields>, <.cdx.> );; DbAdd(<(dbname)>, <(file)>, <(longtablename)>)



using XSharp.VFP
FUNCTION Start () as void
    USE C:\Test\Orders.dbf
    USE customer  && Opens Customer table
    COPY STRUCTURE TO backup && Creates a backup of the Customer table
    USE backup
    APPEND FROM customer FOR country = 'UK'
    //BROWSE FIELDS contact, country
    USE
    DELETE FILE backup.dbf
    wait

function DbAdd(cDb, cFile, cTable    )
    ? cDb, cFile, cTable
    return true


FUNCTION DbCopyXStructFox(cTargetFile AS STRING) AS LOGIC STRICT
    RETURN DbCopyXStruct(cTargetFile)

FUNCTION DbCopyStructFox(cTargetFile AS STRING, acStruct := NULL_ARRAY AS ARRAY, lCopyCdx as LOGIC) AS LOGIC STRICT
    RETURN DbCopyStruct(cTargetFile,acStruct)
