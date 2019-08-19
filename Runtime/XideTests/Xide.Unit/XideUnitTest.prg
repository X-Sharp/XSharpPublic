USING System.Reflection
USING System.Windows.Forms
USING System.Collections.Generic

CLASS XideUnitTest
	STATIC PROTECT oForm AS UnitForm

	STATIC PROPERTY CurrentTest AS TestInfo AUTO
	
	STATIC METHOD Initialize() AS VOID
		oForm := UnitForm{}
		oForm:Show()
	RETURN
	STATIC METHOD Run() AS VOID
		Application.Run(oForm)
	RETURN
	
	STATIC METHOD AddTestsFromAssembly(oAssembly AS Assembly) AS VOID
		FOREACH oType AS Type IN oAssembly:GetTypes()
			IF oType:Name:StartsWith("DbfCdx")
//				LOOP
			END IF
			FOREACH oMethod AS MethodInfo IN oType:GetMethods(BindingFlags.Instance + BindingFlags.Public + BindingFlags.DeclaredOnly)
				IF oMethod:GetCustomAttribute(TypeOf(TestAttribute)) != NULL .or. ;
						oMethod:GetCustomAttribute(TypeOf(FactAttribute)) != NULL
						
					
					LOCAL oTest AS TestInfo
					oTest := TestInfo{}
					oTest:Assembly := oAssembly
					oTest:Type := oType
					oTest:Test := oMethod
					oForm:AddTest(oTest)
					
				END IF
				
			NEXT
			
		NEXT
	RETURN
	
	STATIC METHOD TestRun(lSuccess AS LOGIC, cMessage AS STRING , cFileName AS STRING, nLine AS DWORD) AS VOID
		IF CurrentTest != NULL
			CurrentTest:SubTestRun(lSuccess, cMessage, cFileName, nLine)
		END IF
	RETURN
		
END CLASS

CLASS SubTestInfo
	PROPERTY WasRun AS LOGIC AUTO
	PROPERTY Success AS LOGIC AUTO
	PROPERTY FileName AS STRING AUTO
	PROPERTY Line AS DWORD AUTO
	PROPERTY Message AS STRING AUTO
END CLASS

CLASS TestInfo
	PROPERTY Assembly AS Assembly AUTO
	PROPERTY Type AS Type AUTO
	PROPERTY Test AS MethodInfo AUTO

	PROPERTY WasRun AS LOGIC AUTO
	PROPERTY SubTests AS List<SubTestInfo> AUTO
	PROPERTY Success AS LOGIC AUTO
	CONSTRUCTOR()
		SELF:SubTests := List<SubTestInfo>{}
	RETURN
	
	METHOD Reset() AS VOID
		SELF:WasRun := FALSE
		SELF:Success := TRUE
		SELF:SubTests:Clear()
	RETURN
	
	METHOD CountFailedSubTests() AS INT
		LOCAL nFailed := 0 AS INT
		FOREACH oSubTest AS SubTestInfo IN SELF:SubTests
			IF oSubTest:WasRun .and. .not. oSubTest:Success
				nFailed ++
			END IF
		NEXT
	RETURN nFailed
	
	METHOD SubTestRun(lSuccess AS LOGIC, cMessage AS STRING , cFileName AS STRING, nLine AS DWORD) AS VOID

		SELF:WasRun := TRUE
		SELF:Success := SELF:Success .and. lSuccess

		LOCAL oSubTest AS SubTestInfo
		oSubTest := SubTestInfo{}
		oSubTest:WasRun := TRUE
		oSubTest:Success := lSuccess
		oSubTest:Message := cMessage
		oSubTest:FileName := cFileName
		oSubTest:Line := nLine
		SELF:SubTests:Add(oSubTest)
	RETURN
	
END CLASS



CLASS NUnit.Framework.Assert INHERIT Xide.Unit.Assert
END CLASS

CLASS XUnit.Assert INHERIT Xide.Unit.Assert
END CLASS

CLASS Xide.Unit.Assert
	STATIC PROPERTY Groups AS INT AUTO
	STATIC PROPERTY Passed AS INT AUTO
	STATIC PROPERTY Failed AS INT AUTO
	STATIC METHOD IsNull(o AS OBJECT) AS VOID
		IF o == NULL
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__, GetFileName(ProcFile(1)) , ProcLine(1)
			XideUnitTest.TestRun(FALSE , "IsNull() returned: " + (o==NULL):ToString() , ProcFile(1) , ProcLine(1))
		END IF
		
	RETURN
	STATIC METHOD IsEmpty(o AS STRING) AS VOID
		IF String.IsNullOrEmpty(o)
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__, GetFileName(ProcFile(1)) , ProcLine(1)
			XideUnitTest.TestRun(FALSE , "IsEmpty() returned: " + String.IsNullOrEmpty(o):ToString() , ProcFile(1) , ProcLine(1))
		END IF
		
	RETURN
	STATIC METHOD IsTrue(l AS LOGIC) AS LOGIC
		IF l
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__, GetFileName(ProcFile(1)) , ProcLine(1)
			XideUnitTest.TestRun(FALSE , "IsTrue() returned: " + l:ToString() , ProcFile(1) , ProcLine(1))
		END IF
		
	RETURN l
	STATIC METHOD AreEqual(s1 AS STRING,s2 AS STRING) AS LOGIC
		IF s1 == s2
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__
			XideUnitTest.TestRun(FALSE , "AreEqual() returned: " + s1 + "," + s2 , ProcFile(1) , ProcLine(1))
		END IF
	RETURN s1 == s2
	STATIC METHOD AreEqual(s1 AS DWORD,s2 AS DWORD) AS LOGIC
		IF s1 == s2
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__
			XideUnitTest.TestRun(FALSE , "AreEqual() returned: " + s1:ToString() + "," + s2:ToString() , ProcFile(1) , ProcLine(1))
		END IF
	RETURN s1 == s2
	STATIC METHOD AreEqual(s1 AS INT,s2 AS INT) AS LOGIC
		IF s1 == s2
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__
			XideUnitTest.TestRun(FALSE , "AreEqual() returned: " + s1:ToString() + "," + s2:ToString() , ProcFile(1) , ProcLine(1))
		END IF
	RETURN s1 == s2
	STATIC METHOD AreEqual(o1 AS OBJECT,o2 AS OBJECT) AS VOID
		IF o1:Equals(o2)
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__
			XideUnitTest.TestRun(FALSE , "AreEqual() returned: " + o1:ToString() + "," + o2:ToString() , ProcFile(1) , ProcLine(1))
		END IF
	RETURN
	STATIC METHOD IsFalse(l AS LOGIC) AS LOGIC
		IF .not. l
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__
			XideUnitTest.TestRun(FALSE , "IsFalse() returned: " + l:ToString() , ProcFile(1) , ProcLine(1))
		END IF
	RETURN .not. l


	// XUnit
	STATIC METHOD Equal(o1 AS OBJECT, o2 AS OBJECT) AS VOID
		IF o2 != NULL .and. o2:GetType() == TypeOf(STRING) .and. o1:GetType() == TypeOf(SYMBOL)
			o2 := String2Symbol((STRING)o2)
		END IF
		IF o2 != NULL .and. o2:GetType() == TypeOf(Decimal) .and. o1:GetType() == TypeOf(INT)
			o1 := Decimal{(INT)o1}
		END IF
		IF (o1 == NULL .and. o2 == NULL) .or. (o1 != NULL .and. (o1:Equals(o2) .or. o1:ToString() == o2:ToString() ) )
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__
			XideUnitTest.TestRun(FALSE , "Equal() returned: " + o1:ToString() + " , " + o2:ToString() , ProcFile(1) , ProcLine(1))
		END IF
	RETURN
	STATIC METHOD NotEqual(o1 AS OBJECT, o2 AS OBJECT) AS VOID
		IF (o1 == NULL .and. o2 != NULL) .or. (o1 != NULL .and. .not. o1:Equals(o2))
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__
			XideUnitTest.TestRun(FALSE , "NotEqual() returned: " + o1:ToString() + " , " + o2:ToString() , ProcFile(1) , ProcLine(1))
		END IF
	RETURN
	STATIC METHOD NotEqual(o1 AS IntPtr, o2 AS IntPtr) AS VOID
		IF .not. o1:Equals(o2)
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__
			XideUnitTest.TestRun(FALSE , "NotEqual() returned: " + o1:ToString() + " , " + o2:ToString() , ProcFile(1) , ProcLine(1))
		END IF
	RETURN
	STATIC METHOD @@True(l AS LOGIC) AS VOID
		IF l
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__, GetFileName(ProcFile(1)) , ProcLine(1)
			XideUnitTest.TestRun(FALSE , "True() returned: " + l:ToString() , ProcFile(1) , ProcLine(1))
		END IF
	RETURN
	STATIC METHOD @@False(l AS LOGIC) AS VOID
		IF .not. l
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__, GetFileName(ProcFile(1)) , ProcLine(1)
			XideUnitTest.TestRun(FALSE , "False() returned: " + l:ToString() , ProcFile(1) , ProcLine(1))
		END IF
	RETURN
	STATIC METHOD Throws(e AS Type , o AS System.Action) AS VOID
		#warning Implement Throws
	STATIC METHOD Throws<T>(o AS System.Action) AS VOID
		#warning Implement Throws

	STATIC METHOD Ignore() AS VOID
	STATIC METHOD Fail() AS VOID

	STATIC METHOD GetFileName(cFile AS STRING) AS STRING
		TRY
			cFile := System.IO.FileInfo{cFile}:Name
		END TRY
	RETURN cFile
	
	STATIC METHOD ThrowsAny<t>(o AS Action) AS VOID
		LOCAL lExcption := FALSE AS LOGIC
		TRY
			o:Invoke()
		CATCH
			lExcption := TRUE
		END TRY

		IF lExcption
			Passed ++
			XideUnitTest.TestRun(TRUE , "" , "" , 0)
		ELSE
			Failed ++
			? "failed in ", __ENTITY__, GetFileName(ProcFile(1)) , ProcLine(1)
			XideUnitTest.TestRun(FALSE , "No exception occured : ", ProcFile(1) , ProcLine(1))
		END IF
END CLASS



// Unit test attributes

CLASS TraitAttribute INHERIT System.Attribute
	CONSTRUCTOR(c1 AS STRING, c2 AS STRING)
	RETURN
END CLASS

CLASS FactAttribute INHERIT System.Attribute
END CLASS

CLASS TestFixtureAttribute INHERIT System.Attribute
END CLASS

CLASS TestAttribute INHERIT System.Attribute
END CLASS

CLASS SetupAttribute INHERIT System.Attribute
END CLASS

CLASS ExpectedExceptionAttribute INHERIT System.Attribute
	CONSTRUCTOR(t AS Type)
	RETURN
END CLASS

CLASS CategoryAttribute INHERIT System.Attribute
	CONSTRUCTOR(s AS STRING)
	RETURN
END CLASS

CLASS IgnoreAttribute INHERIT System.Attribute
	CONSTRUCTOR(s AS STRING)
	RETURN
END CLASS


CLASS Vulcan.Error
END CLASS
CLASS Vulcan.Internal.VOStructAttribute
END CLASS

