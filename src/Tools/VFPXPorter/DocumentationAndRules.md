# FabVFPXPorter
## Documentation
Definitions are mostly stored in external files. The Folder structure is the following
~~~
.
+-- VFPXporter.exe
+-- <needed DLLs>
+-- data
|   +--  EventRules.json
|   +--  PropRules.json
|   +--  TypeConvert.json
|   +--  Statements.json
|   +--  VFP2WinForms.json
|   +-templates
|     +-- form
|         +--  prefix.prg
|         +--  StartType.prg
|         +--  InitType.prg
|         +--  EndType.prg
|     +-- designer
|         +--  prefix.prg
|         +--  StartType.prg
|         +--  InitType.prg
|         +--  EndType.prg
|     +-- singlefile
|         +--  prefix.prg
|         +--  StartType.prg
|         +--  InitType.prg
|         +--  EndType.prg
|         +--  prefixNotContainer.prg
|         +--  StartTypeNotContainer.prg
|         +--  InitTypeNotContainer.prg
|         +--  EndTypeNotContainer.prg
|         +--  InitTypeBinding.prg
|     +-- Tools


~~~

## Rules Documentation

All Rules files are JSON files, storing some text information used to translate some VFP code to .NET
#### EventRules.json
Rules used to translate Event and EventHandlers.

The definition has the following schema
~~~
  "<FoxProControlName>": {
    "<FoxProControlEventName>": [ "<WindowsFormsControlEventName", "<WindowsFormsMethodPrototypeWithReturnType>", "<ClassUsedForObjectCreation>" ]
  }
~~~
If in the rules file, a "Common" FoxProControlName exist, these rules will be **added** to all Controls.

So, like in the following sample, we can have something  where _Timer_ is specific to the Timer control, but the Timer Control also receive a definition for _GotFocus_.

- If the _WindowsFormsControlEventName_ ends with **()**, then it is considered as a Method call, so instead of wiring to a Event Handler, the Method is called.  
In this case, the third element will determine the generation. If not empty, the Method is called with 
an object of _ClassUsedForObjectCreation_ type .  
The Constructor is called with two parameters : First is SELF, second is the _event handler_ as string. 

- If it ends with **\{}**, it is considered as a property with an object creation.  
So, _WindowsFormsControlEventName_ will be a property that will received an object of _ClassUsedForObjectCreation_ type .  
The Constructor is called with two parameters : First is SELF, second is the _event handler_ as string. 

- If it ends with **:=**, it is considered as a property setting. The second element will be used as Type for a Cast operation. In all case, the original information will be passed as a double-quoted string.

- In all other cases, it will be considered as a direct event mapping. In the following sample, *Click* will be mapped to Winforms Click event.
In the generated code, the type will be *System.EventHandler*, and the declaration parameters will be as indicated in the second position.
~~~~
{
  "Common": {
    "Click": [ "OnClick", "(sender AS OBJECT, e AS System.EventArgs) AS VOID", "System.EventHandler" ],
    "GotFocus": [ "Enter", "(sender AS OBJECT, e AS System.EventArgs) AS VOID", "System.EventHandler" ],
    "Init": [ "Init()", " AS VOID" ],
    "Refresh": [ "VFPRefresh{}", "() AS USUAL CLIPPER", "VFPOverride" ],
    "AddProperty": [ "VFPAddProperty{}", "() AS USUAL CLIPPER", "VFPOverride" ],
    "Load": [ "VFPLoad()", " AS USUAL" ],
    "Show": [ "VFPShow()", " AS USUAL" ],
    "When": ["Set_When()", "() AS USUAL CLIPPER", "VFPOverride" ],
    "Valid": [ "vfpValid:=", "", "" ]
  },

  "timer": {
    "Timer": [ "Tick", "(sender AS OBJECT, e AS System.EventArgs) AS VOID", "System.EventHandler" ]
  }
}
~~~~

#### PropRules.json
Rules that are used to translate Form and Controls Properties.

We read the Properties of a FoxPro item, and by applying the Rules, we will create a _new_ list of .NET Properties.

The definition has the following schema
~~~
  "<FoxProControlName>": {
    "<FoxProPropertyName>": "<WindowsFormsPropertyWithOptionnalReplacableElements>",
  }
~~~
**_Warning_** These rules are processed Top-Down, so any Rules can change a  _Property_ value, and so its process after that.

If in the rules file, a "Common" FoxProControlName exist, these rules will be **added** to all Controls, **except** to the _Form_ Control.

During process, first we have a Convertion process marked with (C) below, then an Apply process marked (A) below.  
In (C), the _FoxPro_ value will be transformed, if it appears in the rules, by applying the rules.  
In (A), the "new" list of properties (a mix of touch and untouched properties), will written to generated code.

##### Text -> Text
- (C) The _FoxProPropertyName_ will be replaced by the _WindowsFormsPropertyWithOptionnalReplacableElements_.  
For example, with 
~~~~
"Caption": "Text"
~~~~
The FoxPro property _Caption_ will rewrite as _Text_  

#### Property -> Object or Function call
- (C) If _WindowsFormsPropertyWithOptionnalReplacableElements_ ends with **}** or **)**; then the original value is dropped and the property will be set to this replacement value.
   
#### Markers -> Inject other Property value  
- (C) If the _WindowsFormsPropertyWithOptionnalReplacableElements_ contains a special marker : **<@**  and **@>** the content will be used as a place-holder for the corresponding _foxpro property_ value.
Be aware that comparaison/replacement will be made in lowercase !!!  

- (C) If it contains a pair of double question mark **??**, these will be replaced by quotes if needed.  

- (A) If it starts with a double colon **::**, the property affectation will be replaced by a method call as it is in the Rule.

- (A) If it starts with a single colon **:**, the property affectation will be replaced by a method call but it may be applied to a Child item on the Current Item if needed.

- (C)&(A) If the _FoxProPropertyName_ starts with an exclamation mark, the property will be enforce as a **Usual** var. 

- (C) If the _FoxProPropertyName_ starts with a **^** it is considered as a prefix that will be replaced by the _WindowsFormsPropertyWithOptionnalReplacableElements_ element.  
If it contains 99, these will be considered as a number that is extracted and possibly copied into the replacement if this one also contains 99  

- (A) If _WindowsFormsPropertyWithOptionnalReplacableElements_ starts with a **!**, the generated code will Cast the Value as **Usual**   

In these replacement, we have some "special names":  
**@\_CurrentObject\_@** : It can be SELF in a control with no Childs (Form,...), or it can be SELF:ControlVarName with ControlVarName beeing the control that currently manipulated.

##### Examples
So, we can have something like that : 

In the _form_, the _Caption_ property will be converted to _Text_

_ClientSize_ will be a property created by using the FOX properties _Width_ and _Height_

_DoCreate_ property will be removed from the list of Properties

Same for _Top_, _Left_, _Width_ and _Height_ : So, you **MUST** used these values **BEFORE** removing them

All properties starting by _Option_ will be replaced by Buttons(), and any number after _Option_ will be passed as a parameter.  
eg, Option1.Caption will be turned to Buttons(1).Caption  

~~~~
  "Form": {
    "Caption": "Text",
    "ClientSize": "System.Drawing.Size{<@width@>, <@height@>}",
    "DoCreate": "",
    "Top": "",
    "Left": "",
    "Width": "",
    "Height": "",
    "^Option99": "Buttons(99)"
  }
~~~~

#### TypeConvert.json
Rules used to translate ClassNames.

The definition has the following schema
~~~
{
  "<FoxProClassName>": [ "<WindowsFormsClassName>", "<iscontainer>", "<addToControls>" ]
}
~~~
This is a simple One-To-One text replacement, and we have an information that will be used at generation time : is this type a container one ? This information is used in order to use the right generation template.

So, we can have something like that, where _form_ will turn to _System.Windows.Forms.Form_, and _commandbutton_ will turn to _System.Windows.Forms.Button_ and a _container_ to _System.Windows.Forms.Panel_ 
~~~~
{
  "form": ["System.Windows.Forms.Form", "true", "false"],
  "commandbutton": ["System.Windows.Forms.Button", "false", "true"],
  "container": ["System.Windows.Forms.Panel", "true", "true"],
}
~~~~

We have special *\<FoxProClassName>* for Menu strips, Menu items and separator :
+ xsPorterMenuStrip
+ xsPorterMenuItem
+ xsPorterMenuSeparator

#### Statements.json
Simple array with Methods that are sometimes used without the parenthesis.  
You can manually achieve the same process by searching these and add the () after the "keyword"
~~~~
[
  "Refresh",
  "Release",
  "Click"
]
~~~~
Refresh will be replaced by Refresh(), etc  
This Rules are processed *before* the next one.

#### VFP2WinForms.json
A Simple Dictionary with elements that are replaced one-to-one.
~~~~
{
  "this.": "thisObject.",
  "Parent.": "_Parent.",
  "Click()": "_Click()"
}
~~~~
*thisObject* will be created by the VFPXPorter, and refer to the Control/Object that originally contained an EventHandler.  
*_Parent* is a Property of each VFP* Controls, typed as Object to force Late-Bound call


#### Templates and PlaceHolders
_For SCX Files_
In Designer :
	In StartType
		<@formname@>			: Will be replace by the Form Class found in the SCX
		<@supername@>			: Will be replace by the Parent Class, usually the one coming from _TypeConvert.json_
		<@childsDeclaration@>	: Declaration area for all SubItems/Childs of the Form

	In InitType
        <@childsInstantiate@>	: Place where SubItems/Childs are created,
        <@childsInitialize@>    : and their Properties set
		<@addChildsToParent@>	: Place where the SubItems/Childs are added to the Form
		<@formProps@>		: Place where the Form properties are set
        <@userdefProps@>		: Place where the User-Defined Form properties are set

In Form :
	In StartType
		<@formname@>			: Will be replace by the Form Class found in the SCX
		<@supername@>			: Will be replace by the Parent Class, usually the one coming from _TypeConvert.json_
		<@dataenvironment@>		: Declaration area for DataEnvironment and Cursors

	In InitType
        <@InitCall@>			: Place where the Init() method is called if any. You SHOULD not use this as Init() is called during the DO FORM process
		<@setdataenvironment@>	: Place where DataEnvironment and Cursors are Initialized
		<@EventHandlers@>		: Where ALL Event Handlers are placed; those of Controls and Form


_Tools Folder_
These files are used during Project Export.
All files present in this folder will be copied to the Destination folder, and added to the created Project
