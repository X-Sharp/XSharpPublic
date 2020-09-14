USING System.Reflection
USING System.Windows.Forms
USING System.Collections.Generic

FUNCTION Start() AS INT
	LOCAL aTests AS List<STRING>
	LOCAL nSuccess := 0, nFail := 0, nTotal AS INT
	aTests := List<STRING>{};
	{;
	 "C008", "C091", "C114", "C142", "C147", "C151", "C152", "C159", "C160", "C164", ;
	 "C169", "C176", "C195", "C204", "C210", "C235", "C303", "C368" ,"C369", "C376", ;
	 "C377", "C378", "C382", "C383", "C384", "C385", /*"C386",*/ "C387", "C388", "C390", ;
	 "C391", "C392", "C393", "C395", "C396", "C397", "C398", "C401", "C402", "C404", ;
	 "C406", "C410", "C412", "C418", "C420", "C421", "C422", "C423", "C424", "C426", ;
	 /*"C427",*/ "C433", "C434", "C435", "C437", "C441", "C444", "C445", "C446", "C448", ;
	 "C450", "C452", "C457", "C460", "C475", "C478", "C479", "C484", "C499", "C504", ;
	 "C505", "C506", "C507", "C508", "C509", "C515", "C519", "C520", "C521", "C527", ;
	 "C528", "C536", "C538", /*"C541",*/ "C542", "C543", "C548", "C552", "C557", "C558", ;
	 "C560", "C564", "C567", "C573", "C578", "C579", "C582", "C588", "C590", "C591",  ;
 	 "C599", "C602", "C604", "C606", "C607", "C609", "C610", "C611", "C612", "C613", "C615", ;
 	 "C616", "C617", "C618", "C621", "C628", "C629", "C630", "C631", "C632", "C635", ;
	 "C636", "C641", "C643", "C644", "C646", "C647", "C648", "C650", "C651", "C656", ;
	 "C657", "C659", "C661", "C668", "C669", "C671", "C672", "C673", "C675", "C686", ; 
	 "C689", "C697", "C698", "C699", "C709", "C710", "C711", "C712", "C714", "C717", ;
	 "C718", "C323", "C728", "C738",;
	 ;
	 "R678", "R681", "R690", "R698", "R699", "R700" ,"R701", "R702", /*"R705" ,*/"R710",;
	 "R711", "R712", "R725", "R729", "R730", "R735", "R736";
	 }
	 
	#ifdef GUI
	aTests:Add("C386")
	aTests:Add("C541")
	aTests:Add("R705")
	#else
	// they use Environment.CurrentDirectory, which does not work under tthe automated tests. Need to find a better way
	aTests:Remove("C418")
	aTests:Remove("C479")
	aTests:Remove("C521")
	#endif
	 
	
	// TODO Must fail: "C135"
	
	FOREACH cTest AS STRING IN aTests
		TRY
			IF DoTest(cTest)
				nSuccess ++
			ELSE
				? "Failed Runtime Test", cTest
				nFail ++
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
	MessageBox.Show(i"Run {nTotal} tests: {nSuccess} succeeded, {nFail} failed" , "Runtime Tests")
	WAIT
	#endif
RETURN nFail

FUNCTION DoTest(cExe AS STRING) AS LOGIC
	LOCAL lSucces := FALSE AS LOGIC
	LOCAL oAssembly AS Assembly
	? "Running test" , cExe
	oAssembly := Assembly.LoadFile(Application.StartupPath + "\" + cExe + ".exe")
	LOCAL oType AS Type
	oType := oAssembly:GetType(cExe + ".Exe.Functions")
	IF oType == NULL // Core
		oType := oAssembly:GetType("Functions")
	END IF
	LOCAL oMethod AS MethodInfo
	oMethod := oType:GetMethod("Start")
	TRY
		oMethod:Invoke(NULL , NULL)
		lSucces := TRUE
	CATCH e AS Exception
		? e:ToString()
		#ifdef GUI
		System.Windows.Forms.MessageBox.Show(e:ToString() , "Runtime test " + cExe + " failed:")
		#endif
	END TRY                                    
	VoDbCloseAll()
RETURN lSucces

