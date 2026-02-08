===========================
VO SDI Application Template
===========================
This VO SDI Application template is based on the original template that comes with Visual Objects.
We have made some small changes that were needed because the VO SDK Libraries did not compile without changes in .Net:
- The Start.Prg module now declares a new app class "XApp". The App:Start() method has been changed to XApp:Start()
- The Application Icon and bitmap on the Help About screen have been updated
- The SysLink control on the HelpAbout screen now goes to the X# website
- We have included the templates for the VO Editor in the Properties subfolder in the project