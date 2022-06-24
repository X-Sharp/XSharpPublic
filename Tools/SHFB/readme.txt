This folder contains 3 addons to Sandcastle Helpfile Builder that we use to produce the X# documentation:
- XSharpVs has the presentation style
- XSharpSyntax has the code to produce X# style syntax on the topic pages
- XSharpPlugin is a plugin that "post processes" the generated topic pages. It changes METHOD into FUNCTION when relevant and also renames __Usual to USUAL etc.

XSharpVs needs to be deployed to the Sandcastle PresentationStyles folder
The other components need to be desployed to the Sandcastle main folder


To generate links back to the main help file we need a link that looks like this:
<a href="ms-its:XSharp.chm::/command_append_blank.html">APPEND BLANK</a>


