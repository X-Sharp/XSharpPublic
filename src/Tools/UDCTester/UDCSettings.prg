// UDCSettings.prg
// Created by    : robert
// Creation Date : 9/22/2021 2:49:19 PM
// Created for   : 
// WorkStation   : NYX


USING System
USING System.IO
USING System.Collections.Generic
USING System.Text
USING XSharp

BEGIN NAMESPACE UDCTesterApp

	/// <summary>
    /// The UDCSettings class.
    /// </summary>
	CLASS UDCSettings
        PRIVATE CONST DEF_OUTPUTPATH  := "OutputPath" AS STRING
        PRIVATE CONST DEF_PPO         := "WritePPO" AS STRING
        PRIVATE CONST DEF_WRITELEXTOK := "WriteLexTokens" AS STRING
        PRIVATE CONST DEF_WRITEPPOTOK := "WritePPTokens" AS STRING
        PRIVATE CONST DEF_HIDECOMMENTS:= "HideComments" AS STRING
        PRIVATE CONST DEF_HIDEWS      := "HideWs" AS STRING
        PRIVATE CONST DEF_NOSTDDEF    := "NoStdDef" AS STRING
        PRIVATE CONST DEF_DIALECT     := "Dialect" AS STRING
        PRIVATE RegistryKey := Constants.RegistryKey+"\UDCTester" as STRING
        PROPERTY OutPutPath as STRING AUTO
        PROPERTY WritePPo   AS LOGIC AUTO
        PROPERTY WriteLexTokens as LOGIC AUTO
        PROPERTY HideComments as LOGIC AUTO
        PROPERTY HideWhiteSpace as LOGIC AUTO
        PROPERTY WritePPTokens as LOGIC AUTO
        PROPERTY DefaultNoStdDefs as LOGIC AUTO
        PROPERTY DefaultDialect as STRING AUTO
        CONSTRUCTOR()
            LOCAL cFolder as STRING
            cFolder := System.Environment.GetEnvironmentVariable("TEMP")
            cFolder := Path.Combine(cFolder, "UDCTester")
            OutPutPath  := cFolder
            WritePPO    := TRUE
            WriteLexTokens := TRUE
            WritePPTokens   := TRUE
            HideComments    := FALSE
            HideWhiteSpace  := FALSE
            DefaultNoStdDefs := FALSE
            DefaultDialect  := "Core"
            SELF:ReadSettings()
            RETURN
        METHOD ReadSettings() AS VOID
            SELF:OutPutPath     := (String) SELF:ReadSetting(DEF_OUTPUTPATH, SELF:OutPutPath)
            SELF:WritePPO       := SELF:ReadSetting(DEF_PPO, IIF(SELF:WritePPO,1,0)) != 0
            SELF:WriteLexTokens := SELF:ReadSetting(DEF_WRITELEXTOK, IIF(SELF:WriteLexTokens,1,0)) != 0
            SELF:WritePPTokens  := SELF:ReadSetting(DEF_WRITEPPOTOK, IIF(SELF:WritePPTokens,1,0)) != 0
            SELF:HideComments   := SELF:ReadSetting(DEF_HIDECOMMENTS, IIF(SELF:HideComments,1,0)) != 0
            SELF:HideWhiteSpace   := SELF:ReadSetting(DEF_HIDEWS, IIF(SELF:HideWhiteSpace,1,0)) != 0
            SELF:DefaultNoStdDefs := SELF:ReadSetting(DEF_DIALECT, IIF(SELF:DefaultNoStdDefs,1,0)) != 0
            SELF:DefaultDialect := (String) SELF:ReadSetting(DEF_DIALECT, SELF:DefaultDialect)
            
            
        METHOD WriteSettings() AS VOID
            SELF:WriteSetting(DEF_OUTPUTPATH, SELF:OutPutPath)
            SELF:WriteSetting(DEF_PPO, IIF(SELF:WritePPO,1,0))
            SELF:WriteSetting(DEF_WRITELEXTOK, IIF(SELF:WriteLexTokens,1,0))
            SELF:WriteSetting(DEF_WRITEPPOTOK, IIF(SELF:WritePPTokens,1,0))
            SELF:WriteSetting(DEF_HIDECOMMENTS, IIF(SELF:HideComments,1,0))
            SELF:WriteSetting(DEF_HIDEWS, IIF(SELF:HideWhiteSpace,1,0))
            SELF:WriteSetting(DEF_NOSTDDEF, IIF(SELF:DefaultNoStdDefs,1,0))
            SELF:WriteSetting(DEF_DIALECT, SELF:DefaultDialect)

        internal METHOD WriteSetting<T>(name as string, newvalue as T) AS LOGIC

            try
    
                var key := Microsoft.Win32.Registry.CurrentUser
                var subkey := key:OpenSubKey(RegistryKey, true)
                if subkey == null
                
                    subkey := key:CreateSubKey(RegistryKey, true)
                endif
                subkey:SetValue(name, newvalue)
                return true
            catch
                nop
            end try
            return false

                
            internal method ReadSetting(name as string,  defvalue as int) as int
            
                local result := defvalue as int
                try
                    var key := Microsoft.Win32.Registry.CurrentUser
                    var subkey := key:OpenSubKey(RegistryKey, true)
                    if (subkey == null)
                        subkey := key:CreateSubKey(RegistryKey, true)
                    endif
                    var temp := subkey.GetValue(name)
                    if temp == null
                        subkey:SetValue(name, defvalue)
                        result := defvalue
                    else
                        result := (int) temp
                    endif                
                catch
                    result := defvalue
                end try
                return result
                
            internal method ReadSetting(name as string,  defvalue as string) as string
            
                local result := defvalue as string
                try
                    var key := Microsoft.Win32.Registry.CurrentUser
                    var subkey := key:OpenSubKey(RegistryKey, true)
                    if (subkey == null)
                        subkey := key:CreateSubKey(RegistryKey, true)
                    endif
                    var temp := subkey.GetValue(name)
                    if temp == null
                        subkey:SetValue(name, defvalue)
                        result := defvalue
                    else
                        result := (string) temp
                    endif                
                catch
                    result := defvalue
                end try
                return result
            


	END CLASS
END NAMESPACE // UDCTesterApp
