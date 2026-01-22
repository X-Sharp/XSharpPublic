// $safeitemrootname$.prg
// Created by    : $username$
// Creation Date : $time$
// Created for   : $registeredorganization$
// WorkStation   : $machinename$

USING System

FUNCTION Start() AS VOID STRICT
    Console.WriteLine("Hello .Net!")
	Console.WriteLine(System.Environment:Version)
    Console.WriteLine("Press any key to continue...")
    Console.ReadKey()
	
