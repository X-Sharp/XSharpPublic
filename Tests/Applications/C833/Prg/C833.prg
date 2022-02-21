// 833. Parser errors with VFP code
// https://github.com/X-Sharp/XSharpPublic/issues/911
CLASS TestClass
	METHOD Another() AS USUAL
		LOCAL aFiles
		LOCAL M.lcFile
		
		IF VARTYPE(aFiles)="O"
			FOR EACH M.lcFile IN aFiles
				IF File(M.lcFile)
// 					DELETE File (M.lcFile)
				ENDIF
			ENDFOR
		ENDIF
		
	METHOD CheckMe() AS USUAL
		LOCAL liALeft, liATop
		IF TRUE
			IF TRUE
				NOP // <-- Missing ENDIF
			ELSE
				IF liALeft#TRUE OR liATop#TRUE
					NOP
				ENDIF
			ENDIF
		ENDIF
	END METHOD
END CLASS
