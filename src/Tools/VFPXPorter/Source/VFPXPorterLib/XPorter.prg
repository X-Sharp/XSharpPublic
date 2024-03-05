// VFPXPorter.prg
// Created by    : fabri
// Creation Date : 11/16/2020 10:39:27 AM
// Created for   :
// WorkStation   : FABPORTABLE


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING Newtonsoft.Json
USING System.Xml.Serialization
USING System.ComponentModel
USING Serilog
USING Serilog.Core

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// The VFPXPorter class, base class for VFPXPorterVisualBase
    /// </summary>
    ABSTRACT CLASS XPorter IMPLEMENTS IXPorter

        PROPERTY Name AS STRING GET Path.GetFileNameWithoutExtension( SELF:Settings:ItemsPath )

        PROPERTY FileName AS STRING GET Path.GetFileName( SELF:Settings:ItemsPath )

        PROPERTY Settings AS XPorterSettings AUTO

        CONSTRUCTOR( )
            RETURN

        PROPERTY ResultText AS STRING
            GET
                VAR log := BufferedSink.Instance:Log
                BufferedSink.Instance:Clear()
                RETURN log
            END GET
        END PROPERTY

        PROPERTY ErrorText AS STRING
            GET
                VAR log := BufferedSink.Instance:Errors
                RETURN log
            END GET
        END PROPERTY

        VIRTUAL METHOD Initialize( filePath AS STRING, destPath AS STRING, settings AS XPorterSettings  ) AS VOID
            //
            SELF:Settings :=settings
            SELF:Settings:ItemsPath := filePath
            SELF:Settings:OutputPath := destPath
            //
            IF !Directory.Exists( SELF:Settings:OutputPath )
                // Let's try to create the destination Path
                Directory.CreateDirectory( SELF:Settings:OutputPath )
            ENDIF
            RETURN

        ABSTRACT METHOD Analyze( doBackup AS LOGIC ) AS LOGIC

        ABSTRACT METHOD Export( doBackup AS LOGIC ) AS LOGIC


#region Background Worker

            // The background task, if any, that will process the Export process
        PROTECTED _worker AS BackgroundWorker
        PROPERTY Worker AS BackgroundWorker
            SET
                SELF:_worker := VALUE
            END SET
        END PROPERTY
        // The total number of items to process
        PROTECTED _totalItems AS INT
        PROTECTED _progress AS INT

        PROPERTY Canceled AS LOGIC GET IIF( _worker != NULL, _worker:CancellationPending, FALSE )

        METHOD ResetProgress( total AS INT ) AS VOID
            SELF:_progress := 0
            SELF:_totalItems := total

        METHOD UpdateProgress() AS VOID
            IF ( SELF:_worker != NULL )
                SELF:_progress ++
                VAR percentComplete := (INT)Math.Round((REAL8)(100 * _progress) / _totalItems)
                IF percentComplete > 100
                    percentComplete := 100
                ENDIF
                SELF:_worker:ReportProgress(percentComplete)
            ENDIF


#endregion

    END CLASS
END NAMESPACE // VFPXPorterLib