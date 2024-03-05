// VFPDoForm.prg
// Created by    : fabri
// Creation Date : 6/7/2022 10:09:44 PM
// Created for   :
// WorkStation   : FABXPS


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// The VFPDoForm class.
	/// This class is used to "emulate" the DO FORM command
	/// </summary>
	STATIC CLASS __VFPDoForm
		STATIC formParam AS List<Usual>

		STATIC METHOD InitParam() AS VOID
			__VFPDoForm.formParam := List<Usual>{}

		STATIC METHOD Param( p AS USUAL ) AS VOID
			__VFPDoForm.formParam.Add( p )


		STATIC METHOD Create( formName, varName, toVarName, noread, NOSHOW ) AS VOID CLIPPER
			TRY
				LOCAL form AS STRING
				form := formName
				// Remove the extension as sometimes DO FORM is called with formName.scx
				IF form:Contains(".")
					form := Path.GetFileNameWithoutExtension( form )
				ENDIF
				// Call the Constructor with Params if any
				// RvdH you cannot call CreateInstance with the parameters as an array
				// this will send in a first parameter of type USUAL[]
				//__VFPDoForm.formVar := CreateInstance( form, __VFPDoForm.formParam:ToArray())
				// Therefore call CreateInstance with callClipFunc
				// I will add an overload of CreateInstance that takes a USUAL[] later
				var args := List<Usual>{}
				args:Add(form)
				args:AddRange(__VFPDoForm.formParam)
				VAR formVar := _CallClipFunc(#CreateInstance, args:ToArray())
//				IF __VFPDoForm.formParam:Count == 0
//					__VFPDoForm.formVar := CreateInstance( form )
//				ELSE
//					__VFPDoForm.formVar := _CreateInstance( form, __VFPDoForm.formParam:ToArray())
//				ENDIF
				// Set the Var to the created object
				IF !IsNil( varName )
					MemVarPut( varName, formVar )
				ENDIF
				// and Show window
				Send( formVar, "Show" )
				//

			CATCH ex as Exception
				IF ex:InnerException != NULL
					THROW Exception{ "Error in FORM", ex:InnerException }
				ELSE
					THROW Exception{ "Error in DO FORM", ex }
				ENDIF
			END TRY


	END CLASS
END NAMESPACE // XSharp.VFP.UI
