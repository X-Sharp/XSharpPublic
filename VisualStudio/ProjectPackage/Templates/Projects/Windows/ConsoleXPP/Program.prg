// $safeitemrootname$.prg
// Created by    : $username$
// Creation Date : $time$
// Created for   : $registeredorganization$
// WorkStation   : $machinename$


USING System
USING System.Collections.Generic
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$USING System.Text


Procedure Main
    ? "Hello World! Today is ",Date()
    WAIT
    RETURN
