# FabVFPXPorter
## VFP XPorter
The goal is to ease the move of your VFP Application in .NET, using a language you already know.
1. Minimum changes to your code during export
2. Keep the original code as comment if any change is made.
3. Move VFP Forms to Windows Forms, using an adaptation layer/library
4. Move VCX libraries as external projects containing Forms/Controls
5. Create real Windows Forms solution/project so you can use any existing 3rd party for .NET in your application



#### VFP Exporter Application : Current State
1. Export VFP Forms Definition, and convert them to Windows Forms
   - Done...Mostly ;)...Need to enhance the set of Rules
   - Adapt the Binding in order to match the WinForms DataBinding; then use Cursors as DataSource
2. Export VFP EventHandlers, and convert them to Windows Forms EventHandlers
   - Mostly done... Automatic convertion of EventHandlers that are Control-Based in VFP, to Form-Based in .NET
3. Export VFP Projects, and create VisualStudio projects with Libraries, Forms, ...
   - xsproj file is now created, with Forms & .designer.prg in it
   - Injection of Tools Prg in order to provide some features that are not in the Runtime currently 
4. Export VFP Libraries source, and convert to external PRG Files
   - each element is saved into a separate .prg file
5. Added User-Defined elements in export : Fields, Field-Arrays, Methods
    - Help/Documentation is also added at export time
6. Export Menu definitions
    - Menu Event are exported in Menu code (Needs to be tested, I'm looking for samples here)
7. First Report reading and Backup
    - No real export by now (Maybe export to ReportPro ??)


We will also have to take care of Menu definition.

What about Reports ? We may/will have to consider another tool; or introduce here

During 1. step, the Form definition is exported in ~~JSON~~ XML format and source code file (.PRG) is created

Don't forget to have a deep look at the **ExportAdvices.md** document, as some modification cannot be done.

If you want to change the way the Application is doing some transporting, don't forget to have a look at **DocumentationAndRules.md**


#### Todo
(Ideas dropped here just to not forget)
* Use a Browser based on DataGridView and DBF DbDataSource to match the VFP Grid (Work in progress)

#### Warning
Some VFP structures/elements are currently not supported (Should work now !)  

- Support of WITH.. END WITH within a function; for eg :
~~~
WITH something
  code
  .propertyOfSomething = newValue
  functionCall()
  code
END WITH

FUNCTION functionCall()
   .propertyOfSomething = anotherValue
~~~


- Use of # character as != (notEqual)


