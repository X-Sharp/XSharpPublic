//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System
USING System.IO
USING System.Text
USING System.Collections.Generic
USING XSharp.RT
USING XSharp.Internal

///  <exclude/>
FUNCTION _cmdDirMake(cDir as STRING) AS VOID
    System.IO.Directory.CreateDirectory(cDir)

///  <exclude/>
FUNCTION _cmdDirChange(cDir as STRING) AS VOID
    System.IO.Directory.SetCurrentDirectory(cDir)

///  <exclude/>
FUNCTION _cmdDirRemove(cDir as STRING) AS VOID
    System.IO.Directory.Delete(cDir,false)

[NeedsAccessToLocals(FALSE)];
FUNCTION __VfpDir(cCommand AS STRING) AS VOID
    LOCAL cSkeleton AS STRING
    LOCAL cToFile AS STRING
    LOCAL lToPrinter AS LOGIC
    LOCAL nPos AS INT
    // Todo: Implement TO PRINTER clause
    cCommand := cCommand:Trim()
    cSkeleton := cCommand
    cToFile := ""
    lToPrinter := .F.

    nPos := cCommand:IndexOf(" TO FILE ", StringComparison.OrdinalIgnoreCase)
    IF nPos >= 0
        cSkeleton := cCommand:Substring(0, nPos):Trim()
        cToFile := cCommand:Substring(nPos + 9):Trim()
    ELSE
        nPos := cCommand:IndexOf(" TO PRINTER", StringComparison.OrdinalIgnoreCase)
        IF nPos >= 0
            cSkeleton := cCommand:Substring(0, nPos):Trim()
            lToPrinter := .T.
        ENDIF
    ENDIF

    IF cSkeleton:StartsWith("(") .AND. cSkeleton:EndsWith(")")
        TRY
            cSkeleton := (STRING) Evaluate(cSkeleton:Substring(1, cSkeleton:Length - 2))
        CATCH
            NOP
        END TRY
    ENDIF

    IF !String.IsNullOrEmpty(cToFile)
        IF cToFile:StartsWith("(") .AND. cToFile:EndsWith(")")
            TRY
                cToFile := (STRING) Evaluate(cToFile:Substring(1, cToFile:Length - 2))
            CATCH
                cToFile := cToFile:Replace(c'"':ToString(), ""):Replace(c'''':ToString(), "")
            END TRY
        ELSE
            cToFile := cToFile:Replace(c'"':ToString(), ""):Replace(c'''':ToString(), "")
        ENDIF
    ENDIF

    IF cSkeleton:StartsWith("LIKE ", StringComparison.OrdinalIgnoreCase)
        cSkeleton := cSkeleton:Substring(5):Trim()
    ELSEIF cSkeleton:StartsWith("ON ", StringComparison.OrdinalIgnoreCase)
        LOCAL cDrive := cSkeleton:Substring(3):Trim() AS STRING
        IF cDrive:Length == 1 ; cSkeleton := cDrive + ":\*.dbf" ; ENDIF
    ENDIF

    LOCAL cPath := "" AS STRING
    LOCAL cPattern := "" AS STRING

    IF String.IsNullOrEmpty(cSkeleton) .OR. cSkeleton == "*"
        cPath := Directory.GetCurrentDirectory()
        cPattern := "*.dbf"
    ELSE
        IF Directory.Exists(cSkeleton)
            cPath := cSkeleton
            cPattern := "*.dbf"
        ELSE
            TRY
                cPath := Path.GetDirectoryName(cSkeleton)
                cPattern := Path.GetFileName(cSkeleton)
            CATCH
                cPath := ""
                cPattern := cSkeleton
            END TRY
            IF String.IsNullOrEmpty(cPath) ; cPath := Directory.GetCurrentDirectory() ; ENDIF
            IF String.IsNullOrEmpty(cPattern) ; cPattern := "*.dbf" ; ENDIF
        ENDIF
    ENDIF

    LOCAL aFiles AS STRING[]
    TRY
        aFiles := Directory.GetFiles(cPath, cPattern)
    CATCH
        aFiles := STRING[]{0}
    END TRY

    LOCAL sb := StringBuilder{} AS StringBuilder

    IF aFiles:Length == 0
        sb:AppendLine("No matching files found.")
    ELSE
        IF cPattern:EndsWith(".dbf", StringComparison.OrdinalIgnoreCase)
            sb:AppendLine(String.Format("{0,-32} {1,10} {2,20} {3,15}", "Database Table/DBF files", "# Records", "Last Update", "Size"))
            LOCAL nTotalBytes := 0 AS INT64
            FOREACH cFile AS STRING IN aFiles
                LOCAL oInfo := FileInfo{cFile} AS FileInfo
                LOCAL nRecs := 0 AS INT
                LOCAL cDate := "" AS STRING
                TRY
                    USING VAR fs := FileStream{cFile, FileMode.Open, FileAccess.Read, FileShare.ReadWrite}
                    USING VAR br := BinaryReader{fs}
                    br:ReadByte()
                    LOCAL y := br:ReadByte() AS BYTE, m := br:ReadByte() AS BYTE, d := br:ReadByte() AS BYTE
                    nRecs := br:ReadInt32()
                    cDate := String.Format("{0:D2}/{1:D2}/{2:D2}", m, d, y % 100)
                CATCH
                    cDate := oInfo:LastWriteTime:ToString("MM/dd/yy")
                END TRY

                IF oInfo:Name:Length > 25
                    sb:AppendLine(oInfo:Name:ToUpper())
                    sb:AppendLine(String.Format("{0,43} {1,20} {2,15}", nRecs, cDate, oInfo:Length))
                ELSE
                    sb:AppendLine(String.Format("{0,-32} {1,10} {2,20} {3,15}", oInfo:Name:ToUpper(), nRecs, cDate, oInfo:Length))
                ENDIF
                nTotalBytes += oInfo:Length
            NEXT
            sb:AppendLine("")
            sb:AppendLine(String.Format("{0} bytes in {1} files.", nTotalBytes, aFiles:Length))
        ELSE
            sb:AppendLine("Directory of " + cPath + "\" + cPattern)
            sb:AppendLine("")
            LOCAL nCol := 0 AS INT
            FOREACH cFile AS STRING IN aFiles
                sb:Append(String.Format("{0,-16}", Path.GetFileName(cFile):ToUpper()))
                nCol++
                IF nCol >= 4 ; sb:AppendLine() ; nCol := 0 ; ENDIF
            NEXT
            IF nCol > 0 ; sb:AppendLine() ; ENDIF
            sb:AppendLine("")
            sb:AppendLine(String.Format("{0,10} files found.", aFiles:Length))
        ENDIF
    ENDIF

    LOCAL cRes := sb:ToString() AS STRING
    IF !String.IsNullOrEmpty(cToFile)
        File.WriteAllText(cToFile, cRes)
    ELSE
        QOut(cRes)
    ENDIF
    IF lToPrinter
        //Todo Implement printing
        NOP
    ENDIF
RETURN
