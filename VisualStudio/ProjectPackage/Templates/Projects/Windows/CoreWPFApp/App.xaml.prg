USING System
USING System.Collections.Generic
USING System.Configuration
USING System.Data
$if$ ($targetframeworkversion$ >= 3.5)USING System.Linq
$endif$$if$ ($targetframeworkversion$ >= 4.5)USING System.Threading.Tasks
$endif$USING System.Windows


BEGIN NAMESPACE $safeprojectname$

   /// <summary>
   /// Interaction logic for App.xaml
   /// </summary>
   PARTIAL CLASS App INHERIT Application

   END CLASS

END NAMESPACE
