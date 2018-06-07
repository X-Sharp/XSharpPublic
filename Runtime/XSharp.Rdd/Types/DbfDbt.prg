//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

BEGIN NAMESPACE XSharp.RDD
    /// <summary>DBFDBT RDD. For DBF/DBT. No index support at this level</summary>
    CLASS DBFDBT INHERIT DBF
        CONSTRUCTOR
            SUPER()
            SELF:_oMemo := DBTMemo{SELF}
            
        VIRTUAL PROPERTY SysName AS STRING GET TYPEOF(DbfDbt):ToString()
        
        /// <summary>DBT Memo class. Implements the DBT support.</summary>
        // See https://www.clicketyclick.dk/databases/xbase/format/dbt.html#DBT_STRUCT/
        //
        CLASS DBTMemo INHERIT BaseMemo IMPLEMENTS IMemo
            PROTECT _hFile	    AS IntPtr
            PROTECT _FileName   AS STRING
            PROTECT _Shared     AS LOGIC
            PROTECT _ReadOnly   AS LOGIC
            PROTECT _oRDD       AS DBF
            PROTECT _blockSize  AS LONG
            
            CONSTRUCTOR (oRDD AS DBF)
                SUPER(oRDD)
                SELF:_oRdd := oRDD
                SELF:_blockSize := 512
                
                /// <inheritdoc />
            METHOD Flush() 			AS LOGIC		
                THROW NotImplementedException{}
                
                /// <inheritdoc />
            METHOD GetValue(nFldPos AS INT) AS OBJECT
                THROW NotImplementedException{}
                
                /// <inheritdoc />
            METHOD GetValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
                THROW NotImplementedException{}
                
                /// <inheritdoc />
            METHOD GetValueLength(nFldPos AS INT) AS INT
                THROW NotImplementedException{}
                
                /// <inheritdoc />
            VIRTUAL METHOD PutValue(nFldPos AS INT, oValue AS OBJECT) AS LOGIC
                LOCAL objType AS System.Type
                LOCAL objTypeCode AS System.TypeCode
                LOCAL str AS STRING
                // 
                objType := oValue:GetType()
                objTypeCode := Type.GetTypeCode( objType )
                //
                IF ( objTypeCode == TypeCode.Char )
                    str := STRING{ (CHAR)oValue, 1 }
                ELSE
                    str := oValue ASTYPE STRING
                ENDIF
				return true                
                
                /// <inheritdoc />
            VIRTUAL METHOD PutValueFile(nFldPos AS INT, fileName AS STRING) AS LOGIC
                THROW NotImplementedException{}
                
                /// <inheritdoc />
            VIRTUAL METHOD CloseMemFile( ) AS LOGIC
                LOCAL isOk := FALSE AS LOGIC
                IF ( SELF:_hFile != NULL )
                    //
                    TRY
                        isOk := FClose( SELF:_hFile )
                    CATCH
                        isOk := FALSE
                    END TRY
                    SELF:_hFile := NULL
                ENDIF
                RETURN isOk
                
                /// <inheritdoc />
            VIRTUAL METHOD CreateMemFile(info AS DbOpenInfo) AS LOGIC
                LOCAL isOk AS LOGIC
                SELF:_FileName  := info:FileName
                SELF:_FileName  := System.IO.Path.ChangeExtension( SELF:_FileName, DBT_MEMOEXT )
                SELF:_Shared    := info:Shared
                SELF:_ReadOnly  := info:ReadOnly
                //
                SELF:_hFile     := FCreate( SELF:_FileName) 
                isOk := ( SELF:_hFile != F_ERROR )
                IF isOk
                    // Per default, Header Block Size if 512
                    LOCAL memoHeader AS BYTE[]
                    LOCAL nextBlock AS LONG
                    //
					memoheader := byte[]{ 512 }
                    nextBlock := 1
                    Array.Copy(BitConverter.GetBytes(nextBlock),0, memoHeader, 0, SIZEOF(LONG))
                    //
                    FWrite3( SELF:_hFile, memoHeader, 512 )
                ELSE
                    SELF:_oRDD:_DbfError( ERDD.CREATE_MEMO, XSharp.Gencode.EG_CREATE )
                ENDIF
                //
                RETURN isOk
                
                /// <inheritdoc />
            VIRTUAL METHOD OpenMemFile(info AS DbOpenInfo ) AS LOGIC
                LOCAL isOk AS LOGIC
                SELF:_FileName  := info:FileName
                SELF:_FileName  := System.IO.Path.ChangeExtension( SELF:_FileName, DBT_MEMOEXT )
                SELF:_Shared    := info:Shared
                SELF:_ReadOnly  := info:ReadOnly
                //
                SELF:_hFile     := Fopen(SELF:_FileName, info:FileMode)
                isOk := ( SELF:_hFile != F_ERROR )
                IF isOk
                    // Per default, Block Size if 512
                ELSE
                    SELF:_oRDD:_DbfError( ERDD.OPEN_MEMO, XSharp.Gencode.EG_OPEN )
                ENDIF
                //
                RETURN isOk
                
//            PROPERTY NextBlock 	 AS LONG ;
//            GET BitConverter.ToInt32( SELF:_memoBlock, 0);
//            SET Array.Copy(BitConverter.GetBytes(VALUE),0, SELF:_memoBlock, 0, SIZEOF(LONG))
            
            
        END CLASS    
        
    END CLASS
END NAMESPACE
