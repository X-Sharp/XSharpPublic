//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections
BEGIN NAMESPACE XSharp.RDD.CDX
    INTERNAL CLASS VfpCollation
        INTERNAL PROPERTY Name          AS STRING AUTO
        INTERNAL PROPERTY CodePage      AS INT AUTO
        INTERNAL PROPERTY ResourceName  AS STRING AUTO
        INTERNAL PROPERTY WeightTable   AS BYTE[] AUTO
        INTERNAL PROPERTY WorkBuffer    AS BYTE[] AUTO
        INTERNAL PROPERTY HasCombi      AS LOGIC AUTO
        INTERNAL PROPERTY CombiChars    AS BitArray AUTO
        INTERNAL PROPERTY CombiTable    AS BYTE[] AUTO

        INTERNAL CONSTRUCTOR(cName AS STRING, nCodePage AS INT)
            SELF:HasCombi := FALSE
            SELF:ResourceName := upper(cName)+"_"+nCodePage:ToString()
            SELF:WorkBuffer := BYTE[]{512}
            SWITCH Upper(cName)
            CASE "CZECH"
                SELF:HasCombi := TRUE
                SWITCH nCodePage
                CASE 852
                CASE 895
                CASE 1250
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "DUTCH"
                SWITCH nCodePage
                CASE 437
                CASE 850
                CASE 1252
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "GENERAL"
                SWITCH nCodePage
                CASE 437
                CASE 620
                CASE 850
                CASE 852
                CASE 857
                CASE 861
                CASE 865
                CASE 895
                CASE 1252
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "GERMAN"
                SWITCH nCodePage
                CASE 437
                CASE 1252
                    ResourceName := upper(cName)+"_"+nCodePage:ToString()
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "GREEK"
                SWITCH nCodePage
                CASE 737
                CASE 1253
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "HUNGARY"
                SELF:HasCombi := TRUE  
                SWITCH nCodePage
                CASE 852
                CASE 1250
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "ICELAND"
                SWITCH nCodePage
                CASE 437
                CASE 850
                CASE 861
                CASE 1252
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "MACHINE"
                SWITCH nCodePage
                CASE 437
                CASE 620
                CASE 737
                CASE 850
                CASE 852
                CASE 857
                CASE 861
                CASE 865
                CASE 866
                CASE 895
                CASE 1250
                CASE 1251
                CASE 1252
                CASE 1253
                CASE 1254
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "NORDAN"
                SELF:HasCombi := TRUE  
                SWITCH nCodePage
                CASE 437
                CASE 850
                CASE 865
                CASE 1252
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "POLISH"
                SWITCH nCodePage
                CASE 652
                CASE 852
                CASE 1250
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "RUSSIAN"
                SWITCH nCodePage
                CASE 866
                CASE 1251
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "SLOVAK"
                SELF:HasCombi := TRUE  
                SWITCH nCodePage
                CASE 852
                CASE 859
                CASE 1250
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "SPANISH"
                SELF:HasCombi := TRUE  
                SWITCH nCodePage
                CASE 437
                CASE 850
                CASE 1252
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "SWEFIN"
                SWITCH nCodePage
                CASE 437
                CASE 850
                CASE 865
                CASE 1252
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "TURKISH"
                SWITCH nCodePage
                CASE 857
                CASE 1254
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            CASE "UNIQWT"
                SWITCH nCodePage
                CASE 437
                CASE 850
                CASE 1252
                    NOP
                OTHERWISE
                    SELF:Unsupported(cName, nCodePage)
                END SWITCH
            OTHERWISE
                SELF:Unsupported(cName, nCodePage)
            END SWITCH
	        VAR rm := System.Resources.ResourceManager{ "XSharp.RDD.VfpCollations", TYPEOF(Functions):Assembly }
	        VAR obj := rm:GetObject(resourcename+"_COLLATE") 
	        IF obj != NULL
		        VAR bytes := obj ASTYPE BYTE[]
		        IF bytes != NULL
			        SELF:WeightTable := bytes
		        ENDIF
	        ENDIF
            IF SELF:HasCombi
	            obj := rm:GetObject(resourcename+"_COMBI") 
	            IF obj != NULL
		            VAR bytes := obj ASTYPE BYTE[]
		            IF bytes != NULL
			            SELF:CombiTable := bytes
		            ENDIF
                    SELF:CombiChars := System.Collections.BitArray{256}
                    FOR VAR nI := 1 TO SELF:CombiTable:Length STEP 4
                        LOCAL nByte AS BYTE
                        nByte := SELF:CombiTable[nI]
                        SELF:CombiChars[nByte] := TRUE
                    NEXT

	            ENDIF
            ENDIF
            RETURN

        METHOD Unsupported(cName AS STRING, nCodePage AS INT) AS VOID
            THROW Exception{"The combination of Collation '"+cName+"' and Codepage "+nCodePage:ToString()+" is not allowed"}

        METHOD Translate(bytes AS BYTE[]) AS LOGIC
            IF SELF:HasCombi
                RETURN SELF:TranslateCombi(bytes)
            ELSE
                RETURN SELF:TranslateSingle(bytes)
            ENDIF


        METHOD TranslateCombi(bytes AS BYTE[]) AS LOGIC
            LOCAL nI        AS LONG
            LOCAL nLen      AS LONG
            LOCAL bSource   AS BYTE
            LOCAL nTargetOffSet AS LONG
            LOCAL nCombiLength AS LONG
            // The combination table has groups of 4 bytes
            // 0) First Byte
            // 1) Second Byte
            // 2) Replacement single byte when source has first + second byte
            // 3) filler (0)
            nTargetOffSet := 0
            nLen    := bytes:Length /2
            nCombiLength := SELF:CombiTable:Length
            FOR nI := 0 TO  nLen -1
                bSource := bytes[nI]
                IF bSource == 0
                    EXIT
                ENDIF
                IF nI < nLen-1 .AND. SELF:CombiChars[bSource]
                    VAR bNext := bytes[nI+1]
                    FOR VAR nCombi := 0 TO nCombiLength STEP 4
                        VAR nCombi1 := SELF:CombiTable[nCombi]
                        VAR nCombi2 := SELF:CombiTable[nCombi+1]
                        IF bSource == nCombi1 .AND. bNext == nCombi2
                            // Found match 
                            WorkBuffer[nTargetOffSet]   := SELF:CombiTable[nCombi+2]
                            nTargetOffSet += 1
                            nI            += 1 // increment extra because we have just consumed 2 characters
                            LOOP
                        ENDIF
                    NEXT
                ENDIF
                IF ! SELF:TranslateWorker(bSource, REF nTargetOffSet)
                    RETURN FALSE
                ENDIF
            NEXT
            FOR nI := nTargetOffSet TO bytes:Length -1
                Workbuffer[nI] := 0
            NEXT
            System.Array.Copy(Workbuffer, bytes, bytes:Length)            
            RETURN TRUE

        PRIVATE METHOD TranslateWorker(bSource AS BYTE, nTargetOffSet REF LONG) AS LOGIC
            LOCAL nOffSet   AS LONG
            LOCAL bTarget   AS BYTE
            LOCAL bSpecial  AS BYTE
            // Each offset in the weight table contains 8 bytes
            // Some characters are replaced with more than one byte. For example ë gets replaced with the same character as e + 0x04 for the trema
            // 0) original byte
            // 1) replacement byte
            // 2) second replacement byte
            // 3) when non zero then this indicates that the character writes 2 chars
            // 4) 1 for combined characters
            // 5) 2 for combined characters
            // 6 and 7) 6 and 7 are replacement characters. For example for Æ this contains the character a & e and for ß the characters ss
            nOffSet  := bSource
            nOffSet  *= 8
            bTarget  := SELF:WeightTable[nOffSet+0]
            // bSource should be bTarget
            IF bSource != bTarget
                RETURN FALSE
            ENDIF
            bTarget     := SELF:WeightTable[nOffSet+1]
            bSpecial    := SELF:WeightTable[nOffSet+3]
            WorkBuffer[nTargetOffSet] := bTarget
            nTargetOffSet   += 1
            // Check to see if we need to replace the byte with more than one bytes
            IF bSpecial == 1
                bTarget                     := SELF:WeightTable[nOffSet+2]
                WorkBuffer[nTargetOffSet]   := bTarget
                nTargetOffSet               += 1
            ENDIF
            RETURN TRUE

        METHOD TranslateSingle(bytes AS BYTE[]) AS LOGIC
            LOCAL nI        AS LONG
            LOCAL nLen      AS LONG
            LOCAL bSource   AS BYTE
            LOCAL nTargetOffSet AS LONG
            nTargetOffSet := 0
            nLen    := bytes:Length /2
            FOR nI := 0 TO  nLen -1
                bSource  := bytes[nI]
                IF bSource == 0
                    EXIT
                ENDIF
                IF ! SELF:TranslateWorker(bSource, REF nTargetOffSet)
                    RETURN FALSE
                ENDIF
            NEXT
            FOR nI := nTargetOffSet TO bytes:Length -1
                Workbuffer[nI] := 0
            NEXT
            System.Array.Copy(Workbuffer, bytes, bytes:Length)            
            RETURN TRUE

    END CLASS
END NAMESPACE
