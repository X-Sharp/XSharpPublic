// IXPorter.prg
// Created by    : fabri
// Creation Date : 9/24/2023 4:41:01 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel

BEGIN NAMESPACE VFPXPorterLib

    /// <summary>
    /// The IXPorter Interface.
    /// </summary>
    INTERFACE IXPorter

        PROPERTY Settings AS XPorterSettings GET SET

        METHOD Initialize(filePath AS STRING, destPath AS STRING, settings AS XPorterSettings  ) AS VOID

        METHOD Analyze( doBackup := FALSE AS LOGIC ) AS LOGIC

        METHOD Export( doBackup := TRUE AS LOGIC ) AS LOGIC

        PROPERTY ResultText AS STRING GET

        PROPERTY ErrorText AS STRING GET

        PROPERTY Worker AS BackgroundWorker SET

    END INTERFACE
END NAMESPACE // VFPXPorterLib