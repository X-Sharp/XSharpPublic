USING System.Reflection
USING System.Collections.Generic
USING System.Linq

FUNCTION Start() AS INT
	LOCAL aTests AS List<STRING>
	LOCAL nSuccess := 0, nFail := 0, nTotal AS INT
	LOCAL cFailed := "" AS STRING
	aTests := List<STRING>{};
	{;
;
     "D001",  "D003" ;
;
	 }




	FOREACH cTest AS STRING IN aTests:ToArray()
		TRY
			IF DoTest(cTest)
				nSuccess ++
			ELSE
				? "Failed Runtime Test", cTest
				nFail ++
				cFailed += iif(cFailed:Length != 0, ", ", e" :\r\n\r\n") + cTest
			ENDIF
		CATCH e AS Exception
			? e:ToString()
			#ifdef GUI
			MessageBox.Show(e:ToString() , "Could not run test " + cTest)
			#endif
		END TRY
	NEXT
	nTotal := nSuccess + nFail

	?
	? i"Run {nTotal} tests: {nSuccess} succeeded, {nFail} failed"
	?
	#ifdef GUI
	MessageBox.Show(i"Run {nTotal} tests: {nSuccess} succeeded, {nFail} failed{cFailed}" , "Runtime Tests")
	WAIT
	#endif
RETURN nFail

FUNCTION DoTest(cExe AS STRING) AS LOGIC
	LOCAL lSucces := FALSE AS LOGIC
	LOCAL oAssembly AS Assembly
	? "Running test" , cExe
	oAssembly := Assembly.LoadFile(WorkDir() + "\" + cExe + ".dll")
	LOCAL cType := ""  AS STRING
	FOREACH oCustAtt AS CustomAttributeData IN oAssembly:CustomAttributes:ToArray()
	    IF oCustAtt:AttributeType:Name == "ClassLibraryAttribute"
	        cType := (STRING) oCustAtt:ConstructorArguments:First():Value
            EXIT
	    ELSEIF oCustAtt:AttributeType:Name == "VulcanClassLibraryAttribute"
	        cType := (STRING) oCustAtt:ConstructorArguments:First():Value
	        EXIT
	    ENDIF
	NEXT
	LOCAL oType AS Type
    IF String.IsNullOrEmpty(cType)
        cType := cExe + ".Exe.Functions"
    ENDIF
	oType := oAssembly:GetType(cType)
	IF oType == NULL // Core
		oType := oAssembly:GetType("Functions")
	END IF
	LOCAL oMethod AS MethodInfo

	// todo: set the correct dialect by calling
	oMethod := oType:GetMethod("Start",BindingFlags.IgnoreCase+BindingFlags.Static+BindingFlags.Public)
    VAR settings := RuntimeState.GetInstance():Settings
    VAR backup := Dictionary<Set, OBJECT>{}
    FOREACH VAR entry IN settings
        backup:Add(entry:Key, entry:Value)
    NEXT
	TRY
	    IF oMethod == NULL
	        ? "Could not find Start method in assembly "+oAssembly:GetName():FullName
    		lSucces := FALSE
	    ELSE
	        VAR pars := oMethod:GetParameters()
	        IF pars:Length == 0
    	       oMethod:Invoke(NULL , NULL)
	        ELSE
	            VAR oPars := OBJECT[]{pars:Length}
	            oMethod:Invoke(NULL , oPars)
	        ENDIF
		lSucces := TRUE
	    ENDIF
	CATCH e AS Exception
		? e:ToString()
		#ifdef GUI
		System.Windows.Forms.MessageBox.Show(e:ToString() , "Runtime test " + cExe + " failed:")
		#endif
	END TRY
	settings:Clear()
    FOREACH VAR entry IN backup
        settings:Add(entry:Key, entry:Value)
    NEXT


RETURN lSucces

