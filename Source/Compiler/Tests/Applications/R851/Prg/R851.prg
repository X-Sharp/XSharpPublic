USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.VFP

#pragma warnings(9073, off) // undeclared, field or memvar
#pragma warnings(9092, off) // undeclared, variable or cursor
FUNCTION Start() AS VOID STRICT
    ? "Testing Late bound property get with ""."" accessor operator"
    undec = SubClass{}
    ? undec:ThisForm == undec.ThisForm
    ? undec:ThisForm:Content == undec.ThisForm.Content
    WAIT
    RETURN

BEGIN NAMESPACE TestThisForm

	/// <summary>
	/// The BaseClass class.
	/// </summary>
	CLASS BaseClass

		PUBLIC content AS STRING

		PROPERTY ThisForm AS OBJECT GET SELF

		CONSTRUCTOR()
			RETURN

	END CLASS

CLASS SubClass INHERIT BaseClass

		CONSTRUCTOR()
			this.Init()
			RETURN


		METHOD Init() AS VOID
			thisform.Content = "content"

	END CLASS

END NAMESPACE // TestThisForm
