USING System.IO
USING System.Runtime.InteropServices
USING System.Text
USING System.Collections.Generic
FUNCTION Start AS VOID
	VAR files := Directory.GetFiles("c:\cavo28SP3\Nations","*.dll")
	VAR aDefines := ReadDefines()
	FOREACH VAR sFile IN files
		? sFile 
		LOCAL hLib AS IntPtr
		LOCAL hRes AS IntPtr
		LOCAL hTmp AS IntPtr
		LOCAL hData AS IntPtr
		LOCAL pBytes AS BYTE[]
		hLib 	:= Win32.LoadLibrary(sFile)
		IF hLib == IntPtr.Zero
			? "Error", Win32.GetLastError()
		ELSE

			hRes 	:= Win32.FindResource(hLib, "#111",10)
			IF hRes != IntPtr.Zero
				hTmp 	:= Win32.LoadResource(hLib, hRes)
				IF hTmp != IntPtr.Zero
					hData 	:= Win32.LockResource(hTmp)
					IF (hData != IntPtr.Zero)                      
						pBytes := BYTE[]{256}
						Marshal.Copy(hData,pBytes,0,256)
						VAR sOut := Path.ChangeExtension(sFile, "Collation")
						System.IO.File.WriteAllBytes(sOut, pBytes)						
						? "   "+sOut
					ENDIF 
					Win32.FreeResource(hTmp)         
				ENDIF         
				LOCAL sb := StringBuilder{1024}	AS STringBuilder
				LOCAL result := StringBuilder{}	AS STringBuilder
				
				// Find the strings in the strintable
				FOR VAR i := 1 TO 65636
					VAR len := Win32.LoadString(hLib, i, sb, sb:Capacity)
					IF (len > 0)  
						IF aDefines:ContainsKey(i)
							result:AppendLine(String.Format("{0} = {1}", aDefines[i], sb:ToString():Replace(_chr(10),"\n")))
						ELSE	                                                           			
							result:AppendLine(String.Format("{0} = {1}", i, sb:ToString():Replace(_chr(10),"\n")))
						ENDIF
					ENDIF                    
				NEXT
				VAR sStringTable := Path.ChangeExtension(sFile, "Strings")
				System.IO.File.WriteAllText(sStringTable, result:ToString())    
				? "   "+sStringTable
			ENDIF 
			Win32.FreeLibrary(hLib)
		ENDIF
	NEXT
	
	



STATIC CLASS Win32
	[DllImport("kernel32.dll")];
	STATIC METHOD LoadLibrary(lpLibFileName AS STRING) AS IntPtr
	[DllImport("kernel32.dll", CharSet:=CharSet.Unicode, EntryPoint := "FindResourceW", SetLastError := TRUE)];
	STATIC METHOD FindResource(hModule AS IntPtr, lpName AS STRING, lpType AS INT) AS IntPtr 

	[DllImport("kernel32.dll", EntryPoint := "FindResourceW", SetLastError := TRUE)];
	STATIC METHOD FindResource(hModule AS IntPtr, lpName AS IntPtr, lpType AS INT) AS IntPtr 

	[DllImport("Kernel32.dll", EntryPoint := "LockResource")];
	STATIC METHOD LockResource(hGlobal AS IntPtr) AS IntPtr
	
	[DllImport("Kernel32.dll", EntryPoint := "LoadResource", SetLastError := TRUE)];
	STATIC METHOD LoadResource(hModule AS IntPtr, hResInfo AS IntPtr) AS IntPtr
		
	[DllImport("Kernel32.dll", EntryPoint := "SizeofResource", SetLastError := TRUE)];
	STATIC METHOD SizeOfResource(hModule AS IntPtr, hResource AS IntPtr) AS DWORD
	
	[DllImport("Kernel32.dll", EntryPoint := "FreeResource", SetLastError := TRUE)];
	STATIC METHOD FreeResource(hResource AS IntPtr) AS LOGIC

	[DllImport("kernel32.dll")];
	STATIC METHOD FreeLibrary(hLibModule AS IntPtr) AS LOGIC

	[DllImport("kernel32.dll")];
	STATIC METHOD GetLastError() AS LONG STRICT

	[DllImport("User32.dll")];
	STATIC METHOD LoadString(hInstance AS IntPtr, uId AS LONG, lpBuffer AS StringBuilder, nBufferMax AS INT) AS INT 

		
END CLASS	

FUNCTION ReadDefines AS Dictionary<INT, STRING>
	VAR aLines := File.ReadAllLines("c:\cavo28SP3\Nations\VoError.h")
	VAR aDict := Dictionary<INT, STRING>{aLines:Length}
	FOREACH VAR sLine IN aLines
		LOCAL sTemp := sLine:Trim():ToUpper() AS STRING
		DO WHILE sTemp:Contains(e"\t")
			sTemp := sTemp:Replace(e"\t"," ")
		ENDDO
		DO WHILE sTemp:Contains("  ")
			sTemp := sTemp:Replace("  "," ")
		ENDDO
		IF sTemp != NULL .and. sTemp:Startswith("#DEFINE")       
			VAR aElements := sTemp:Split(" +":ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
			IF aELements:Length > 1    
				// element 1 = the #define
				VAR sDef := aElements[2]
				VAR sNum := aElements[3]
				LOCAL iVal AS INT32    
				IF Int32.TryParse(sNum, OUT iVal)
					IF sTemp:IndexOf("+") > 0
						// Get the rest of the elements				
						sTemp := sTemp:SubString(sTemp:IndexOf("+")+1 )
						aElements := sTemp:Split("+":ToCharArray(), StringSplitOptions.RemoveEmptyEntries)
						FOREACH VAR sSub IN aElements
							LOCAL iSub AS INT    
							IF Int32.TryParse(sSub:Trim(), OUT iSub)
								iVal += iSub
							ENDIF
						NEXT
					ENDIF
					IF !aDict:ContainsKey(iVal) 
						aDict:Add(iVal, sDef)
					ELSE 
						? "Duplicate key", iVal
					ENDIF
				ENDIF
			ENDIF
		ENDIF
	NEXT
	RETURN aDict
	
	
