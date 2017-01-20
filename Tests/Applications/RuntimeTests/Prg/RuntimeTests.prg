#using System.Reflection
#using System.Windows.Forms

FUNCTION Start() AS VOID
	LOCAL aTests AS STRING[]
	LOCAL nSuccess,nFail,nTotal AS INT
	aTests := <STRING>;
	{"C008", "C091", "C114", "C142", "C147", "C151", "C152", "C159", "C160", "C164", ;
	 "C169", "C176", "C195", "C204", "C210", "C235", "C303", "C369", "C376", "C377", ;
	 "C378", "C382", "C383", "C384", "C385", "C386", "C387", "C388", "C390", "C391", ;
	 "C392", "C393", "C395", "C396", "C398", "C401", "C402", "C404", "C406"}
	
	// TODO Must fail: "C135"
	
	FOREACH cTest AS STRING IN aTests
		TRY
			IF DoTest(cTest)
				nSuccess ++
			ELSE
				nFail ++
			ENDIF
		CATCH e AS Exception
			MessageBox.Show(e:ToString() , "Could not run test " + cTest)
		END TRY
	NEXT
	nTotal := nSuccess + nFail

	MessageBox.Show(i"Run {nTotal} tests: {nSuccess} succeeded, {nFail} failed" , "Runtime Tests")
RETURN

FUNCTION DoTest(cExe AS STRING) AS LOGIC
	LOCAL lSucces := FALSE AS LOGIC
	LOCAL oAssembly AS Assembly
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
		System.Windows.Forms.MessageBox.Show(e:ToString() , "Runtime test " + cExe + " failed:")
	END TRY
RETURN lSucces

