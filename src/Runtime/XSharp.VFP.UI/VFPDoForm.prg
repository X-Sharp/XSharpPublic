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
		/// <summary>
		/// Accumulated constructor arguments for the next <see cref="Create"/> call. Reset by <see cref="InitParam"/>.
		/// </summary>
		STATIC formParam AS List<Usual>

		/// <summary>
		/// Clears the constructor parameter list before a new <c>DO FORM</c> sequence.
		/// </summary>
		STATIC METHOD InitParam() AS VOID
			__VFPDoForm.formParam := List<Usual>{}

		/// <summary>
		/// Appends a single constructor argument to the parameter list.
		/// </summary>
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
				// Spaces in the original SCX filename become underscores in the generated class name
				form := form:Replace(" ", "_")
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

	/// <summary>
	/// Runtime helper for the VFP <c>DO &amp;macro [WITH …]</c> command.<br/>
	/// Call <see cref="InitParam"/> to clear the parameter list, then zero or more <see cref="Param"/>
	/// calls to push arguments, then <see cref="Execute"/> to dispatch.<br/>
	/// <see cref="Execute"/> strips the path and extension from the path string to obtain the procedure
	/// name, then calls it via <c>_CallClipFunc</c> with the accumulated parameters so the call is
	/// resolved at runtime.
	/// </summary>
	STATIC CLASS __VFPDo
		/// <summary>Accumulated arguments for the next <see cref="Execute"/> call. Reset by <see cref="InitParam"/>.</summary>
		STATIC doParam AS List<Usual>

		/// <summary>Clears the parameter list before a new <c>DO &amp;macro</c> sequence.</summary>
		STATIC METHOD InitParam() AS VOID
			__VFPDo.doParam := List<Usual>{}

		/// <summary>Appends a single argument to the parameter list.</summary>
		STATIC METHOD Param(p AS USUAL) AS VOID
			__VFPDo.doParam:Add(p)

		/// <summary>
		/// Resolves <paramref name="cPath"/> to a procedure name (strips path and extension),
		/// then calls it with the accumulated <see cref="doParam"/> list.
		/// </summary>
		STATIC METHOD Execute(cPath AS STRING) AS USUAL
			TRY
				VAR procName := Path.GetFileNameWithoutExtension(cPath)
				VAR args := __VFPDo.doParam:ToArray()
				IF args:Length == 0
					RETURN Eval(MCompile(procName + "()"))
				ELSE
					RETURN _CallClipFunc(String2Symbol(procName), args)
				ENDIF
			CATCH ex AS Exception
				THROW Exception{"Error in DO &macro", ex}
			END TRY
	END CLASS

END NAMESPACE // XSharp.VFP.UI
