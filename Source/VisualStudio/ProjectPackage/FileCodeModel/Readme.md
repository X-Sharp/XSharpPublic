## FileCodeModel Implementation


Classes in the FileCodeModel folder are exposing some tools to access to create Code elements.  
It is use, at least, by the XamlDesigner to GoTo Events and to create events.

#### XSharpFileCodeModel
The access point is the XSharpFileCodeModel class.  
An instance is returned by the OAXSharpFileItem.

This class will then create a CodeDomProvider. Using it, it will parse the corresponding file and fill the CodeCompileUnit.  
Then, the XamlDesigner call the CodeElments property to retrieve informations.

#### CodeDomCodeElements
This is a Collection of CodeDomCodeElement.  
It is linked to the XSharpFileCodeModel through the Parent Property

#### CodeDomCodeElement
This is the base type of code elements.  


### How the XamlDesigner adds an Event ?

After creating a XSharpFileCodeModel; and calling the CodeDomCodeElements property, it will search the right method (Namespace;Class;MethodName;Parameters types).  
If it doesn't exist; it will get the **CodeDomCodeClass** object, then call the *AddFunction()* method to create it.
It will then call the *AddParameter()* for each needed parameter, and then show the code.
**Notice that nothing will indicate that the creation of the event is completed !!**  
So, it will do the following process :
~~~~
// Add Event and parameters
CodeFunction newFunc = CreateFunction(codeBehindType, codeEvent, methodName, location);
// Move the caret to the Edit Point
TextPoint startPoint = newFunc.GetStartPoint();
TextPoint endPoint = newFunc.GetEndPoint();
EditPoint edtPt = startPoint.CreateEditPoint();
edtPt.ReplaceText((object)endPoint, initialStatements, 0);
edtPt.SmartFormat(endPoint);
~~~~

What we are doing now, is that when the *GetStartPoint()* is called, the previous Event/Parameters creation is saved. 
Then the file is reloaded & reparsed; a new CodeCompileUnit will be created, and location is retrieved from there, and returned.  
For the *GetEndPoint()*, it will get the StartPoint value and add one to the X position.

_**VisualStudio has introduced some breaking changes in the EnvDTE.EditPoint interface, the effect was that the returned instance was not the same and the ReplaceText() was crashing**_
To avoid that, a zrapper called **CodeDomEditPoint** has been created. It catches the exception in *ReplaceText()* to make the process work.



