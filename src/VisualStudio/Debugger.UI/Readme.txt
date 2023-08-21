Reading state from the current running process was much harder than we anticipated.
At this moment we have a special method "ExecExpressionAsync" in the support class 
that we pass a string with an expression that needs to be evaluated in the context
of the running app. This method returns the result as a string, surrounded with quotes.
That makes it hard to read structured information from the process, such as a list of
globals or settings.


To simplify this we have created a special XSharp.Debugger.Support.Dll that will be 
dynamically loaded in the process that we are debugging. This DLL has some special
entry points that we call to get the information.
- XSharp.Debugger.Support.RtLink.GetGlobals()
- XSharp.Debugger.Support.RtLink.GetSettings()
- XSharp.Debugger.Support.RtLink.GetWorkareas()
- XSharp.Debugger.Support.RtLink.GetMemVars()
These entry points can return a string that contains name/value pairs for things we are interested in. 

- XSharp.Debugger.Support.RtLink.IsRTLoaded()
This returns a string indicating if the runtime is loaded in the process.

This DLL is compiled for .Net 4.0, so it can be used from most applications.

At this moment the DLL has 4 different objects that we return to the debugger:
- GlobalItem
- MemVarItem
- SetingsItem
- WorkareaItem
Each of these objects also has an associated collection. 
That collection has a Serialize and Deserialize method.
The Serialize method is called insize the debuggee and returns a string with the values
from the collection.
Inside the debugger the starting and ending quote are removed and then the string is 
deserialized into a collection again.
To make this easier the XSharp.Debugger.Support.DLL is also linked into the Debugger.UI library, 
so we can be sure that the class definitions used inside the debuggee match the class definitions
inside the debugger.
We did not choose to use Json or XML to serialize because that adds some (unneeded) overhead to the strings.

Inside the Debugger.Support DLL we use reflection to find the various entry points in the runtime.
Because some of the data types are a bit "complex" we have also used Dynamic in a few places, so we get
"late binding" support delivered by Microsoft. Of course this is then case sensitive.
This allows us to fairly easy walk a dictionary without having to worry about how to decode the items in the dictionary.


If we want we can also read more info such as the dialect or some of the other settings.
