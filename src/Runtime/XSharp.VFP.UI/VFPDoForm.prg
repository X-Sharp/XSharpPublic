// VFPDoForm.prg
//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.


USING System
USING System.Collections.Generic
USING System.Text
USING System.IO

BEGIN NAMESPACE XSharp.VFP.UI

	/// <summary>
	/// Static helper that implements the VFP <c>DO FORM</c> command.<br/>
	/// Generated code calls <see cref="InitParam"/> to clear the parameter list, then zero or more
	/// <see cref="Param"/> calls to push constructor arguments, then <see cref="Create"/> to
	/// instantiate the form class by name (via <c>_CallClipFunc(#CreateInstance, …)</c>),
	/// optionally store it in a variable via <c>MemVarPut</c>, and finally call <c>Show()</c> on it.
	/// </summary>
	STATIC CLASS __VFPDoForm
		/// <summary>Accumulated constructor arguments for the next <see cref="Create"/> call. Reset by <see cref="InitParam"/>.</summary>
		STATIC formParam AS List<Usual>

		/// <summary>Clears the constructor parameter list before a new <c>DO FORM</c> sequence.</summary>
		STATIC METHOD InitParam() AS VOID
			__VFPDoForm.formParam := List<Usual>{}

		/// <summary>Appends a single constructor argument to the parameter list.</summary>
		STATIC METHOD Param( p AS USUAL ) AS VOID
			__VFPDoForm.formParam.Add( p )


		/// <summary>
		/// Creates and shows the form class named <paramref name="formName"/> (extension stripped if present),
		/// passing the accumulated <see cref="formParam"/> list as constructor arguments.
		/// If <paramref name="varName"/> is not <c>NIL</c>, stores the new instance in the named memory variable.
		/// </summary>
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
                // and Show window (We should take car of the NOSHOW option here,
                // but for now we will always show the form)
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
