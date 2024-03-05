// IXPorterWorker.prg
// Created by    : fabri
// Creation Date : 10/4/2023 10:49:57 AM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.ComponentModel

BEGIN NAMESPACE VFPXPorterLib

    INTERFACE IXPorterWorker

        PROPERTY Canceled AS LOGIC GET

        PROPERTY Worker AS BackgroundWorker SET

        METHOD ResetProgress( total AS INT ) AS VOID

        METHOD UpdateProgress() AS VOID


    END INTERFACE
END NAMESPACE
