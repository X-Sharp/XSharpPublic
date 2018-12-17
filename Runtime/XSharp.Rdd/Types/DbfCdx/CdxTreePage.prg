// CdxBlock.prg
// Created by    : fabri
// Creation Date : 10/25/2018 10:43:18 PM
// Created for   : 
// WorkStation   : FABPORTABLE

USING System
USING System.Collections.Generic
USING System.Text
USING System.IO
USING System.Runtime.CompilerServices

BEGIN NAMESPACE XSharp.RDD.CDX

	/// <summary>
	/// The CdxPageBase class.
	/// </summary>
	INTERNAL CLASS CdxTreePage INHERIT CdxPage
	    PROTECTED INTERNAL CONSTRUCTOR( fileHandle AS IntPtr, nPage as Int32 )
            SUPER(fileHandle, nPage)
		RETURN

        PROPERTY NodeAttribute as CdxNodeAttribute ;
          GET (CdxNodeAttribute) _GetWord(CDXPAGE_NODEATTR) ;
          SET _SetWord(CDXPAGE_NODEATTR, value), isHot := TRUE

    PROTECTED CONST CDXPAGE_NODEATTR	:= 0	AS WORD // WORD


	END CLASS
END NAMESPACE 
