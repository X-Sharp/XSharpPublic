VOSTRUCT _WinPhoneButtonInfo ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwButtonMode AS DWORD
	MEMBER dwButtonFunction AS DWORD

	MEMBER dwButtonTextSize AS DWORD
	MEMBER dwButtonTextOffset AS DWORD

	MEMBER dwDevSpecificSize AS DWORD
	MEMBER dwDevSpecificOffset AS DWORD

	MEMBER dwButtonState AS DWORD
VOSTRUCT _WinPhoneExtensionID ALIGN 1
	MEMBER dwExtensionID0 AS DWORD
	MEMBER dwExtensionID1 AS DWORD
	MEMBER dwExtensionID2 AS DWORD
	MEMBER dwExtensionID3 AS DWORD
VOSTRUCT _WinPhoneCaps ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwProviderInfoSize AS DWORD
	MEMBER dwProviderInfoOffset AS DWORD

	MEMBER dwPhoneInfoSize AS DWORD
	MEMBER dwPhoneInfoOffset AS DWORD

	MEMBER dwPermanentPhoneID AS DWORD
	MEMBER dwPhoneNameSize AS DWORD
	MEMBER dwPhoneNameOffset AS DWORD
	MEMBER dwStringFormat AS DWORD

	MEMBER dwPhoneStates AS DWORD
	MEMBER dwHookSwitchDevs AS DWORD
	MEMBER dwHandsetHookSwitchModes AS DWORD
	MEMBER dwSpeakerHookSwitchModes AS DWORD
	MEMBER dwHeadsetHookSwitchModes AS DWORD

	MEMBER dwVolumeFlags AS DWORD
	MEMBER dwGainFlags AS DWORD
	MEMBER dwDisplayNumRows AS DWORD
	MEMBER dwdisplayNumColumns AS DWORD
	MEMBER dwNumRingModes AS DWORD
	MEMBER dwNumButtonLamps AS DWORD

	MEMBER dwButtonModesSize AS DWORD
	MEMBER dwButtonModesOffset AS DWORD

	MEMBER dwButtonFunctionsSize AS DWORD
	MEMBER dwButtonFunctionsOffset AS DWORD

	MEMBER dwLampModesSize AS DWORD
	MEMBER dwLampModesOffset AS DWORD

	MEMBER dwNumSetData AS DWORD
	MEMBER dwSetDataSize AS DWORD
	MEMBER dwSetDataOffset AS DWORD

	MEMBER dwNumGetData AS DWORD
	MEMBER dwGetDataSize AS DWORD
	MEMBER dwGetDataOffset AS DWORD

	MEMBER dwDevSpecificSize AS DWORD
	MEMBER dwDevSpecificOffset AS DWORD   

	// RvdH 070411 added	
	//#if (TAPI_CURRENT_VERSION >= 0x00020000)
	MEMBER dwDeviceClassesSize AS DWORD                            // TAPI v2.0
	MEMBER dwDeviceClassesOffset AS DWORD                          // TAPI v2.0
	MEMBER dwPhoneFeatures AS DWORD                                // TAPI v2.0
	MEMBER dwSettableHandsetHookSwitchModes AS DWORD               // TAPI v2.0
	MEMBER dwSettableSpeakerHookSwitchModes AS DWORD               // TAPI v2.0
	MEMBER dwSettableHeadsetHookSwitchModes AS DWORD               // TAPI v2.0
	MEMBER dwMonitoredHandsetHookSwitchModes AS DWORD              // TAPI v2.0
	MEMBER dwMonitoredSpeakerHookSwitchModes AS DWORD              // TAPI v2.0
	MEMBER dwMonitoredHeadsetHookSwitchModes AS DWORD              // TAPI v2.0
	//#endif

	//#if (TAPI_CURRENT_VERSION >= 0x00020002)
	MEMBER PermanentPhoneGuid IS _WINGUID                             // TAPI v2.2
	//#endif

VOSTRUCT _WinPhoneStatus ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwStatusFlags AS DWORD
	MEMBER dwNumOwners AS DWORD
	MEMBER dwNumMonitors AS DWORD
	MEMBER dwRingMode AS DWORD
	MEMBER dwRingVolume AS DWORD

	MEMBER dwHandsetHookSwitchMode AS DWORD
	MEMBER dwHandsetVolume AS DWORD
	MEMBER dwHandsetGain AS DWORD

	MEMBER dwSpeakerHookSwitchMode AS DWORD
	MEMBER dwSpeakerVolume AS DWORD
	MEMBER dwSpeakerGain AS DWORD

	MEMBER dwHeadsetHookSwitchMode AS DWORD
	MEMBER dwHeadsetVolume AS DWORD
	MEMBER dwHeadsetGain AS DWORD

	MEMBER dwDisplaySize AS DWORD
	MEMBER dwDisplayOffset AS DWORD

	MEMBER dwLampModesSize AS DWORD
	MEMBER dwLampModesOffset AS DWORD

	MEMBER dwOwnerNameSize AS DWORD
	MEMBER dwOwnerNameOffset AS DWORD

	MEMBER dwDevSpecificSize AS DWORD
	MEMBER dwDevSpecificOffset AS DWORD   
	MEMBER dwPhoneFeatures	AS DWORD 	// RvdH 070411 added
VOSTRUCT _WinVarString ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwStringFormat AS DWORD
	MEMBER dwStringSize AS DWORD
	MEMBER dwStringOffset AS DWORD
VOSTRUCT _WinLineAddressCaps ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwLineDeviceID AS DWORD

	MEMBER dwAddressSize AS DWORD
	MEMBER dwAddressOffset AS DWORD

	MEMBER dwDevSpecificSize AS DWORD
	MEMBER dwDevSpecificOffset AS DWORD

	MEMBER dwAddressSharing AS DWORD
	MEMBER dwAddressStates AS DWORD
	MEMBER dwCallInfoStates AS DWORD
	MEMBER dwCallerIDFlags AS DWORD
	MEMBER dwCalledIDFlags AS DWORD
	MEMBER dwConnectedIDFlags AS DWORD
	MEMBER dwRedirectionIDFlags AS DWORD
	MEMBER dwRedirectingIDFlags AS DWORD
	MEMBER dwCallStates AS DWORD
	MEMBER dwDialToneModes AS DWORD
	MEMBER dwBusyModes AS DWORD
	MEMBER dwSpecialInfo AS DWORD
	MEMBER dwDisconnectModes AS DWORD

	MEMBER dwMaxNumActiveCalls AS DWORD
	MEMBER dwMaxNumOnHoldCalls AS DWORD
	MEMBER dwMaxNumOnHoldPendingCalls AS DWORD
	MEMBER dwMaxNumConference AS DWORD
	MEMBER dwMaxNumTransConf AS DWORD

	MEMBER dwAddrCapFlags AS DWORD
	MEMBER dwCallFeatures AS DWORD
	MEMBER dwRemoveFromConfCaps AS DWORD
	MEMBER dwRemoveFromConfState AS DWORD
	MEMBER dwTransferModes AS DWORD
	MEMBER dwParkModes AS DWORD

	MEMBER dwForwardModes AS DWORD
	MEMBER dwMaxForwardEntries AS DWORD
	MEMBER dwMaxSpecificEntries AS DWORD
	MEMBER dwMinFwdNumRings AS DWORD
	MEMBER dwMaxFwdNumRings AS DWORD

	MEMBER dwMaxCallCompletions AS DWORD
	MEMBER dwCallCompletionConds AS DWORD
	MEMBER dwCallCompletionModes AS DWORD
	MEMBER dwNumCompletionMessages AS DWORD
	MEMBER dwCompletionMsgTextEntrySize AS DWORD
	MEMBER dwCompletionMsgTextSize AS DWORD
	MEMBER dwCompletionMsgTextOffset AS DWORD
	MEMBER dwAddressFeatures AS DWORD    

	// RvdH 070411 added	
	//#if (TAPI_CURRENT_VERSION >= 0x00020000)
	MEMBER dwPredictiveAutoTransferStates AS DWORD                 // TAPI v2.0
	MEMBER dwNumCallTreatments AS DWORD                            // TAPI v2.0
	MEMBER dwCallTreatmentListSize AS DWORD                        // TAPI v2.0
	MEMBER dwCallTreatmentListOffset AS DWORD                      // TAPI v2.0
	MEMBER dwDeviceClassesSize AS DWORD                            // TAPI v2.0
	MEMBER dwDeviceClassesOffset AS DWORD                          // TAPI v2.0
	MEMBER dwMaxCallDataSize AS DWORD                              // TAPI v2.0
	MEMBER dwCallFeatures2 AS DWORD                                // TAPI v2.0
	MEMBER dwMaxNoAnswerTimeout AS DWORD                           // TAPI v2.0
	MEMBER dwConnectedModes AS DWORD                               // TAPI v2.0
	MEMBER dwOfferingModes AS DWORD                                // TAPI v2.0
	MEMBER dwAvailableMediaModes AS DWORD										// TAPI v2.0
	//#endif

VOSTRUCT _WinLineAddressStatus ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwNumInUse AS DWORD
	MEMBER dwNumActiveCalls AS DWORD
	MEMBER dwNumOnHoldCalls AS DWORD
	MEMBER dwNumOnHoldPendCalls AS DWORD
	MEMBER dwAddressFeatures AS DWORD

	MEMBER dwNumRingsNoAnswer AS DWORD
	MEMBER dwForwardNumEntries AS DWORD
	MEMBER dwForwardSize AS DWORD
	MEMBER dwForwardOffset AS DWORD

	MEMBER dwTerminalModesSize AS DWORD
	MEMBER dwTerminalModesOffset AS DWORD

	MEMBER dwDevSpecificSize AS DWORD
	MEMBER dwDevSpecificOffset AS DWORD
VOSTRUCT _WinLineDialParams ALIGN 1
	MEMBER dwDialPause AS DWORD
	MEMBER dwDialSpeed AS DWORD
	MEMBER dwDigitDuration AS DWORD
	MEMBER dwWaitForDialtone AS DWORD
VOSTRUCT _WinLineCallInfo ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER hLine AS PTR
	MEMBER dwLineDeviceID AS DWORD
	MEMBER dwAddressID AS DWORD

	MEMBER dwBearerMode AS DWORD
	MEMBER dwRate AS DWORD
	MEMBER dwMediaMode AS DWORD

	MEMBER dwAppSpecific AS DWORD
	MEMBER dwCallID AS DWORD
	MEMBER dwRelatedCallID AS DWORD
	MEMBER dwCallParamFlags AS DWORD
	MEMBER dwCallStates AS DWORD

	MEMBER dwMonitorDigitModes AS DWORD
	MEMBER dwMonitorMediaModes AS DWORD
	MEMBER DialParams IS _WinLineDialParams

	MEMBER dwOrigin AS DWORD
	MEMBER dwReason AS DWORD
	MEMBER dwCompletionID AS DWORD
	MEMBER dwNumOwners AS DWORD
	MEMBER dwNumMonitors AS DWORD

	MEMBER dwCountryCode AS DWORD
	MEMBER dwTrunk AS DWORD

	MEMBER dwCallerIDFlags AS DWORD
	MEMBER dwCallerIDSize AS DWORD
	MEMBER dwCallerIDOffset AS DWORD
	MEMBER dwCallerIDNameSize AS DWORD
	MEMBER dwCallerIDNameOffset AS DWORD

	MEMBER dwCalledIDFlags AS DWORD
	MEMBER dwCalledIDSize AS DWORD
	MEMBER dwCalledIDOffset AS DWORD
	MEMBER dwCalledIDNameSize AS DWORD
	MEMBER dwCalledIDNameOffset AS DWORD

	MEMBER dwConnectedIDFlags AS DWORD
	MEMBER dwConnectedIDSize AS DWORD
	MEMBER dwConnectedIDOffset AS DWORD
	MEMBER dwConnectedIDNameSize AS DWORD
	MEMBER dwConnectedIDNameOffset AS DWORD

	MEMBER dwRedirectionIDFlags AS DWORD
	MEMBER dwRedirectionIDSize AS DWORD
	MEMBER dwRedirectionIDOffset AS DWORD
	MEMBER dwRedirectionIDNameSize AS DWORD
	MEMBER dwRedirectionIDNameOffset AS DWORD

	MEMBER dwRedirectingIDFlags AS DWORD
	MEMBER dwRedirectingIDSize AS DWORD
	MEMBER dwRedirectingIDOffset AS DWORD
	MEMBER dwRedirectingIDNameSize AS DWORD
	MEMBER dwRedirectingIDNameOffset AS DWORD

	MEMBER dwAppNameSize AS DWORD
	MEMBER dwAppNameOffset AS DWORD

	MEMBER dwDisplayableAddressSize AS DWORD
	MEMBER dwDisplayableAddressOffset AS DWORD

	MEMBER dwCalledPartySize AS DWORD
	MEMBER dwCalledPartyOffset AS DWORD

	MEMBER dwCommentSize AS DWORD
	MEMBER dwCommentOffset AS DWORD

	MEMBER dwDisplaySize AS DWORD
	MEMBER dwDisplayOffset AS DWORD

	MEMBER dwUserUserInfoSize AS DWORD
	MEMBER dwUserUserInfoOffset AS DWORD

	MEMBER dwHighLevelCompSize AS DWORD
	MEMBER dwHighLevelCompOffset AS DWORD

	MEMBER dwLowLevelCompSize AS DWORD
	MEMBER dwLowLevelCompOffset AS DWORD

	MEMBER dwChargingInfoSize AS DWORD
	MEMBER dwChargingInfoOffset AS DWORD

	MEMBER dwTerminalModesSize AS DWORD
	MEMBER dwTerminalModesOffset AS DWORD

	MEMBER dwDevSpecificSize AS DWORD
	MEMBER dwDevSpecificOffset AS DWORD    
	// RvdH 070411 added	
	//#if (TAPI_CURRENT_VERSION >= 0x00020000)
	MEMBER dwCallTreatment AS DWORD                                // TAPI v2.0
	MEMBER dwCallDataSize AS DWORD                                 // TAPI v2.0
	MEMBER dwCallDataOffset AS DWORD                               // TAPI v2.0
	MEMBER dwSendingFlowspecSize AS DWORD                          // TAPI v2.0
	MEMBER dwSendingFlowspecOffset AS DWORD                        // TAPI v2.0
	MEMBER dwReceivingFlowspecSize AS DWORD                        // TAPI v2.0
	MEMBER dwReceivingFlowspecOffset AS DWORD                      // TAPI v2.0
	//#endif

	
VOSTRUCT _WinLineCallList ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwCallsNumEntries AS DWORD
	MEMBER dwCallsSize AS DWORD
	MEMBER dwCallsOffset AS DWORD
VOSTRUCT _WinLineCallParams ALIGN 1					 // Defaults:
	MEMBER dwTotalSize AS DWORD				 // ---------

	MEMBER dwBearerMode AS DWORD 			 // voice
	MEMBER dwMinRate AS DWORD				 // (3.1kHz)
	MEMBER dwMaxRate AS DWORD				 // (3.1kHz)
	MEMBER dwMediaMode AS DWORD				 // interactiveVoice

	MEMBER dwCallParamFlags AS DWORD 		 // 0
	MEMBER dwAddressMode AS DWORD			 // addressID
	MEMBER dwAddressID AS DWORD				 // (any available)

	MEMBER DialParams IS _WinLineDialParams	 // (0, 0, 0, 0)

	MEMBER dwOrigAddressSize AS DWORD		 // 0
	MEMBER dwOrigAddressOffset AS DWORD
	MEMBER dwDisplayableAddressSize AS DWORD
	MEMBER dwDisplayableAddressOffset AS DWORD

	MEMBER dwCalledPartySize AS DWORD		 // 0
	MEMBER dwCalledPartyOffset AS DWORD

	MEMBER dwCommentSize AS DWORD			 // 0
	MEMBER dwCommentOffset AS DWORD

	MEMBER dwUserUserInfoSize AS DWORD		 // 0
	MEMBER dwUserUserInfoOffset AS DWORD

	MEMBER dwHighLevelCompSize AS DWORD		 // 0
	MEMBER dwHighLevelCompOffset AS DWORD

	MEMBER dwLowLevelCompSize AS DWORD		 // 0
	MEMBER dwLowLevelCompOffset AS DWORD

	MEMBER dwDevSpecificSize AS DWORD		 // 0
	MEMBER dwDevSpecificOffset AS DWORD             
	// RvdH 070411 added
	//#if (TAPI_CURRENT_VERSION >= 0x00020000)
	MEMBER dwPredictiveAutoTransferStates AS DWORD                 // TAPI v2.0
	MEMBER dwTargetAddressSize AS DWORD                            // TAPI v2.0
	MEMBER dwTargetAddressOffset AS DWORD                          // TAPI v2.0
	MEMBER dwSendingFlowspecSize AS DWORD                          // TAPI v2.0
	MEMBER dwSendingFlowspecOffset AS DWORD                        // TAPI v2.0
	MEMBER dwReceivingFlowspecSize AS DWORD                        // TAPI v2.0
	MEMBER dwReceivingFlowspecOffset AS DWORD                      // TAPI v2.0
	MEMBER dwDeviceClassSize AS DWORD                              // TAPI v2.0
	MEMBER dwDeviceClassOffset AS DWORD                            // TAPI v2.0
	MEMBER dwDeviceConfigSize AS DWORD                             // TAPI v2.0
	MEMBER dwDeviceConfigOffset AS DWORD                           // TAPI v2.0
	MEMBER dwCallDataSize AS DWORD                                 // TAPI v2.0
	MEMBER dwCallDataOffset AS DWORD                               // TAPI v2.0
	MEMBER dwNoAnswerTimeout AS DWORD                              // TAPI v2.0
	MEMBER dwCallingPartyIDSize AS DWORD                           // TAPI v2.0
	MEMBER dwCallingPartyIDOffset AS DWORD                         // TAPI v2.0
	//#endif


VOSTRUCT _WinLineCallStatus ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwCallState AS DWORD
	MEMBER dwCallStateMode AS DWORD
	MEMBER dwCallPrivilege AS DWORD
	MEMBER dwCallFeatures AS DWORD

	MEMBER dwDevSpecificSize AS DWORD
	MEMBER dwDevSpecificOffset AS DWORD   
	// RvdH 070411 added
	//#if (TAPI_CURRENT_VERSION >= 0x00020000)
	MEMBER dwCallFeatures2 AS DWORD           // TAPI v2.0
	MEMBER tStateEntryTime IS _WINSYSTEMTIME  // TAPI v2.0
	//#endif

VOSTRUCT _WinLineExtensionID ALIGN 1
	MEMBER dwExtensionID0 AS DWORD
	MEMBER dwExtensionID1 AS DWORD
	MEMBER dwExtensionID2 AS DWORD
	MEMBER dwExtensionID3 AS DWORD
VOSTRUCT _WinLineDevCaps ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwProviderInfoSize AS DWORD
	MEMBER dwProviderInfoOffset AS DWORD

	MEMBER dwSwitchInfoSize AS DWORD
	MEMBER dwSwitchInfoOffset AS DWORD

	MEMBER dwPermanentLineID AS DWORD
	MEMBER dwLineNameSize AS DWORD
	MEMBER dwLineNameOffset AS DWORD
	MEMBER dwStringFormat AS DWORD

	MEMBER dwAddressModes AS DWORD
	MEMBER dwNumAddresses AS DWORD
	MEMBER dwBearerModes AS DWORD
	MEMBER dwMaxRate AS DWORD
	MEMBER dwMediaModes AS DWORD

	MEMBER dwGenerateToneModes AS DWORD
	MEMBER dwGenerateToneMaxNumFreq AS DWORD
	MEMBER dwGenerateDigitModes AS DWORD
	MEMBER dwMonitorToneMaxNumFreq AS DWORD
	MEMBER dwMonitorToneMaxNumEntries AS DWORD
	MEMBER dwMonitorDigitModes AS DWORD
	MEMBER dwGatherDigitsMinTimeout AS DWORD
	MEMBER dwGatherDigitsMaxTimeout AS DWORD

	MEMBER dwMedCtlDigitMaxListSize AS DWORD
	MEMBER dwMedCtlMediaMaxListSize AS DWORD
	MEMBER dwMedCtlToneMaxListSize AS DWORD
	MEMBER dwMedCtlCallStateMaxListSize AS DWORD

	MEMBER dwDevCapFlags AS DWORD
	MEMBER dwMaxNumActiveCalls AS DWORD
	MEMBER dwAnswerMode AS DWORD
	MEMBER dwRingModes AS DWORD
	MEMBER dwLineStates AS DWORD

	MEMBER dwUUIAcceptSize AS DWORD
	MEMBER dwUUIAnswerSize AS DWORD
	MEMBER dwUUIMakeCallSize AS DWORD
	MEMBER dwUUIDropSize AS DWORD
	MEMBER dwUUISendUserUserInfoSize AS DWORD
	MEMBER dwUUICallInfoSize AS DWORD

	MEMBER MinDialParams IS _WinLineDialParams
	MEMBER MaxDialParams IS _WinLineDialParams
	MEMBER DefaultDialParams IS _WinLineDialParams

	MEMBER dwNumTerminals AS DWORD
	MEMBER dwTerminalCapsSize AS DWORD
	MEMBER dwTerminalCapsOffset AS DWORD
	MEMBER dwTerminalTextEntrySize AS DWORD
	MEMBER dwTerminalTextSize AS DWORD
	MEMBER dwTerminalTextOffset AS DWORD

	MEMBER dwDevSpecificSize AS DWORD
	MEMBER dwDevSpecificOffset AS DWORD
	MEMBER dwLineFeatures AS DWORD   
	// RvdH 070411 added
	//#if (TAPI_CURRENT_VERSION >= 0x00020000)
	MEMBER dwSettableDevStatus AS DWORD                            // TAPI v2.0
	MEMBER dwDeviceClassesSize AS DWORD                            // TAPI v2.0
	MEMBER dwDeviceClassesOffset AS DWORD                          // TAPI v2.0
	//#endif

	//#if (TAPI_CURRENT_VERSION >= 0x00020002)
	MEMBER PermanentLineGuid IS _WINGUID                              // TAPI v2.2
	//#endif



VOSTRUCT _WinLineDevStatus ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwNumOpens AS DWORD
	MEMBER dwOpenMediaModes AS DWORD
	MEMBER dwNumActiveCalls AS DWORD
	MEMBER dwNumOnHoldCalls AS DWORD
	MEMBER dwNumOnHoldPendCalls AS DWORD
	MEMBER dwLineFeatures AS DWORD
	MEMBER dwNumCallCompletions AS DWORD
	MEMBER dwRingMode AS DWORD
	MEMBER dwSignalLevel AS DWORD
	MEMBER dwBatteryLevel AS DWORD
	MEMBER dwRoamMode AS DWORD

	MEMBER dwDevStatusFlags AS DWORD

	MEMBER dwTerminalModesSize AS DWORD
	MEMBER dwTerminalModesOffset AS DWORD

	MEMBER dwDevSpecificSize AS DWORD
	MEMBER dwDevSpecificOffset AS DWORD    
	// RvdH 070411 added
	//#if (TAPI_CURRENT_VERSION >= 0x00020000)
	MEMBER dwAvailableMediaModes AS DWORD                          // TAPI v2.0
	MEMBER dwAppInfoSize AS DWORD                                  // TAPI v2.0
	MEMBER dwAppInfoOffset AS DWORD                                // TAPI v2.0
	//#endif

VOSTRUCT _WinLineForward ALIGN 1
	MEMBER dwForwardMode AS DWORD

	MEMBER dwCallerAddressSize AS DWORD
	MEMBER dwCallerAddressOffset AS DWORD

	MEMBER dwDestCountryCode AS DWORD
	MEMBER dwDestAddressSize AS DWORD
	MEMBER dwDestAddressOffset AS DWORD   

VOSTRUCT _WinLineForwardList  ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNumEntries AS DWORD
	MEMBER DIM ForwardList[1] IS _WinLineForward  	// RvdH 070411 changed from DWORD to _WinLineForward
VOSTRUCT _WinLineGenerateTone ALIGN 1
	MEMBER dwFrequency AS DWORD
	MEMBER dwCadenceOn AS DWORD
	MEMBER dwCadenceOff AS DWORD
	MEMBER dwVolume AS DWORD
VOSTRUCT _WinLineMediaControlCallState ALIGN 1
	MEMBER dwCallStates AS DWORD
	MEMBER dwMediaControl AS DWORD
VOSTRUCT _WinLineMediaControlDigit ALIGN 1
	MEMBER dwDigit AS DWORD
	MEMBER dwDigitModes AS DWORD
	MEMBER dwMediaControl AS DWORD
VOSTRUCT _WinLineMediaControlMedia ALIGN 1
	MEMBER dwMediaModes AS DWORD
	MEMBER dwDuration AS DWORD
	MEMBER dwMediaControl AS DWORD
VOSTRUCT _WinLineMediaControlTone ALIGN 1
	MEMBER dwAppSpecific AS DWORD
	MEMBER dwDuration AS DWORD
	MEMBER dwFrequency1 AS DWORD
	MEMBER dwFrequency2 AS DWORD
	MEMBER dwFrequency3 AS DWORD
	MEMBER dwMediaControl AS DWORD
VOSTRUCT _WinLineMonitorTone ALIGN 1
	MEMBER dwAppSpecific AS DWORD
	MEMBER dwDuration AS DWORD
	MEMBER dwFrequency1 AS DWORD
	MEMBER dwFrequency2 AS DWORD
	MEMBER dwFrequency3 AS DWORD
VOSTRUCT _WinLineReqMakeCall ALIGN 1
	MEMBER DIM szDestAddress[TAPIMAXDESTADDRESSSIZE] AS BYTE
	MEMBER DIM szAppName[TAPIMAXAPPNAMESIZE] AS BYTE
	MEMBER DIM szCalledParty[TAPIMAXCALLEDPARTYSIZE] AS BYTE
	MEMBER DIM szComment[TAPIMAXCOMMENTSIZE] AS BYTE
VOSTRUCT _WinLineReqMediaCall ALIGN 1
	MEMBER hWnd AS PTR
	MEMBER wRequestID AS DWORD
	MEMBER DIM szDeviceClass[TAPIMAXDEVICECLASSSIZE] AS BYTE
	MEMBER DIM ucDeviceID[TAPIMAXDEVICEIDSIZE] AS BYTE
	MEMBER dwSize AS DWORD
	MEMBER dwSecure AS DWORD
	MEMBER DIM szDestAddress[TAPIMAXDESTADDRESSSIZE] AS BYTE
	MEMBER DIM szAppName[TAPIMAXAPPNAMESIZE] AS BYTE
	MEMBER DIM szCalledParty[TAPIMAXCALLEDPARTYSIZE] AS BYTE
	MEMBER DIM szComment[TAPIMAXCOMMENTSIZE] AS BYTE
VOSTRUCT _WinLineTermCaps ALIGN 1
	MEMBER dwTermDev AS DWORD
	MEMBER dwTermModes AS DWORD
	MEMBER dwTermSharing AS DWORD
VOSTRUCT _WinLineTranslateOutput ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwDialableStringSize AS DWORD
	MEMBER dwDialableStringOffset AS DWORD
	MEMBER dwDisplayableStringSize AS DWORD
	MEMBER dwDisplayableStringOffset AS DWORD

	MEMBER dwCurrentCountry AS DWORD
	MEMBER dwDestCountry AS DWORD
	MEMBER dwTranslateResults AS DWORD
VOSTRUCT _WinLineTranslateCaps ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwNumLocations AS DWORD
	MEMBER dwLocationListSize AS DWORD
	MEMBER dwLocationListOffset AS DWORD

	MEMBER dwCurrentLocationID AS DWORD

	MEMBER dwNumCards AS DWORD
	MEMBER dwCardListSize AS DWORD
	MEMBER dwCardListOffset AS DWORD

	MEMBER dwCurrentPreferredCardID AS DWORD
VOSTRUCT _WinLineLocationEntry ALIGN 1
	MEMBER dwPermanentLocationID AS DWORD
	MEMBER dwLocationNameSize AS DWORD
	MEMBER dwLocationNameOffset AS DWORD
	MEMBER dwCountryCode AS DWORD
	MEMBER dwCityCodeSize AS DWORD
	MEMBER dwCityCodeOffset AS DWORD
	MEMBER dwPreferredCardID AS DWORD
	MEMBER dwLocalAccessCodeSize AS DWORD
	MEMBER dwLocalAccessCodeOffset AS DWORD
	MEMBER dwLongDistanceAccessCodeSize AS DWORD
	MEMBER dwLongDistanceAccessCodeOffset AS DWORD
	MEMBER dwTollPrefixListSize AS DWORD
	MEMBER dwTollPrefixListOffset AS DWORD
	MEMBER dwCountryID AS DWORD
	MEMBER dwOptions AS DWORD
	MEMBER dwCancelCallWaitingSize AS DWORD
	MEMBER dwCancelCallWaitingOffset AS DWORD
VOSTRUCT _WinLineCardEntry ALIGN 1
	MEMBER dwPermanentCardID AS DWORD
	MEMBER dwCardNameSize AS DWORD
	MEMBER dwCardNameOffset AS DWORD
	MEMBER dwCardNumberDigits AS DWORD
	MEMBER dwSameAreaRuleSize AS DWORD
	MEMBER dwSameAreaRuleOffset AS DWORD
	MEMBER dwLongDistanceRuleSize AS DWORD
	MEMBER dwLongDistanceRuleOffset AS DWORD
	MEMBER dwInternationalRuleSize AS DWORD
	MEMBER dwInternationalRuleOffset AS DWORD
	MEMBER dwOptions AS DWORD
VOSTRUCT _WinLineCountryList ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD

	MEMBER dwNumCountries AS DWORD
	MEMBER dwCountryListSize AS DWORD
	MEMBER dwCountryListOffset AS DWORD
VOSTRUCT _WinLineCountryEntry ALIGN 1
	MEMBER dwCountryID AS DWORD
	MEMBER dwCountryCode AS DWORD
	MEMBER dwNextCountryID AS DWORD
	MEMBER dwCountryNameSize AS DWORD
	MEMBER dwCountryNameOffset AS DWORD
	MEMBER dwSameAreaRuleSize AS DWORD
	MEMBER dwSameAreaRuleOffset AS DWORD
	MEMBER dwLongDistanceRuleSize AS DWORD
	MEMBER dwLongDistanceRuleOffset AS DWORD
	MEMBER dwInternationalRuleSize AS DWORD
	MEMBER dwInternationalRuleOffset AS DWORD
VOSTRUCT _WinLineProviderList ALIGN 1
	MEMBER dwTotalSize AS DWORD
	MEMBER dwNeededSize AS DWORD
	MEMBER dwUsedSize AS DWORD
	MEMBER dwNumProviders AS DWORD
	MEMBER dwProviderListSize AS DWORD
	MEMBER dwProviderListOffset AS DWORD
VOSTRUCT _WinLineProviderEntry ALIGN 1
	MEMBER dwPermanentProviderID AS DWORD
	MEMBER dwProviderFilenameSize AS DWORD
	MEMBER dwProviderFilenameOffset AS DWORD
_DLL FUNCTION tapiRequestMakeCall( ;
		lpszDestAddress AS PSZ, ;
		lpszAppName AS PSZ, ;
		lpszCalledParty AS PSZ, ;
		lpszComment AS PSZ) AS LONG PASCAL:tapi32.tapiRequestMakeCall
_DLL FUNCTION lineRegisterRequestRecipient( ;
		hLineApp AS DWORD PTR, ;
		dwRegistrationInstance AS DWORD, ;
		dwRequestMode AS DWORD, ;
		bEnable AS DWORD) AS LONG PASCAL:tapi32.lineRegisterRequestRecipient
_DLL FUNCTION tapiGetLocationInfo( ;
		lpszConutryCode AS PSZ, ;
		lpszCityCode AS PSZ) AS LONG PASCAL:tapi32.tapiGetLocationInfo
_DLL FUNCTION lineSetCurrentLocation( ;
		hLineApp AS DWORD PTR, ;
		dwLocation AS DWORD) AS LONG PASCAL:tapi32.lineSetCurrentLocation
_DLL FUNCTION lineTranslateAddress( ;
		hLineApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		dwAPIVersion AS DWORD, ;
		lpszAddressIn AS PSZ, ;
		dwCard AS DWORD, ;
		dwTranslateOptions AS DWORD, ;
		lpTranslateOutput AS _WinLineTranslateOutput) AS LONG PASCAL:tapi32.lineTranslateAddress
_DLL FUNCTION lineAccept( ;
		hCall AS DWORD PTR, ;
		lpsUserUserInfo AS PSZ, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.lineAccept
_DLL FUNCTION lineAddToConference( ;
		hConfCall AS DWORD PTR, ;
		hConsultCall AS DWORD PTR) AS LONG PASCAL:tapi32.lineAddToConference
_DLL FUNCTION lineAnswer( ;
		hCall AS DWORD PTR, ;
		lpsUserUserInfo AS PSZ, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.lineAnswer
_DLL FUNCTION lineBlindTransfer( ;
		hCall AS DWORD PTR, ;
		lpszDestAddress AS PSZ, ;
		dwCountryCode AS DWORD) AS LONG PASCAL:tapi32.lineBlindTransfer
_DLL FUNCTION lineClose( ;
		hLine AS DWORD PTR) AS LONG PASCAL:tapi32.lineClose
_DLL FUNCTION lineCompleteCall( ;
		hCall AS DWORD PTR, ;
		lpdwCompletionID AS DWORD PTR, ;
		dwCompletionMode AS DWORD, ;
		dwMessageID AS DWORD) AS LONG PASCAL:tapi32.lineCompleteCall
_DLL FUNCTION lineCompleteTransfer( ;
		hCall AS DWORD PTR, ;
		hConsultCall AS DWORD PTR, ;
		lphConfCall AS DWORD PTR, ;
		dwTransferMode AS DWORD) AS LONG PASCAL:tapi32.lineCompleteTransfer
_DLL FUNCTION lineConfigDialog( ;
		dwDeviceID AS DWORD, ;
		hwndOwner AS DWORD PTR, ;
		lpszDeviceClass AS PSZ) AS LONG PASCAL:tapi32.lineConfigDialog
_DLL FUNCTION lineConfigDialogEdit( ;
		dwDeviceID AS DWORD, ;
		hwndOwner AS DWORD PTR, ;
		lpszDeviceClass AS PSZ, ;
		lpDeviceConfigIn AS PTR, ;
		dwSize AS DWORD, ;
		lpDeviceConfigOut AS _WinVarString) AS LONG PASCAL:tapi32.lineConfigDialogEdit
_DLL FUNCTION lineDeallocateCall( ;
		hCall AS DWORD PTR) AS LONG PASCAL:tapi32.lineDeallocateCall
_DLL FUNCTION lineDevSpecific( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		hCall AS DWORD PTR, ;
		lpParams AS PTR, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.lineDevSpecific
_DLL FUNCTION lineDevSpecificFeature( ;
		hLine AS DWORD PTR, ;
		dwFeature AS DWORD, ;
		lpParams AS PTR, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.lineDevSpecificFeature
_DLL FUNCTION lineDial( ;
		hCall AS DWORD PTR, ;
		lpszDestAddress AS PSZ, ;
		dwCountryCode AS DWORD) AS LONG PASCAL:tapi32.lineDial
_DLL FUNCTION lineDrop( ;
		hCall AS DWORD PTR, ;
		lpsUserUserInfo AS PSZ, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.lineDrop
_DLL FUNCTION lineForward( ;
		hLine AS DWORD PTR, ;
		bAllAddresses AS DWORD, ;
		dwAddressID AS DWORD, ;
		lpForwardList AS _WinLineForwardList, ;
		dwNumRingsNoAnswer AS DWORD, ;
		lphConsultCall AS DWORD PTR, ;
		lpCallParams AS _WinLineCallParams) AS LONG PASCAL:tapi32.lineForward
_DLL FUNCTION lineGatherDigits(;
		hCall AS DWORD PTR, ;
		dwDigitModes AS DWORD, ;
		lpsDigits AS PSZ, ;
		dwNumDigits AS DWORD, ;
		lpszTerminationDigits AS PSZ, ;
		dwFirstDigitTimeout AS DWORD, ;
		dwInterDigitTimeout AS DWORD) AS LONG PASCAL:tapi32.lineGatherDigits
_DLL FUNCTION lineGenerateDigits( ;
		hCall AS DWORD PTR, ;
		dwDigitMode AS DWORD, ;
		lpszDigits AS PSZ, ;
		dwDuration AS DWORD) AS LONG PASCAL:tapi32.lineGenerateDigits
_DLL FUNCTION lineGenerateTone( ;
		hCall AS DWORD PTR, ;
		dwToneMode AS DWORD, ;
		dwDuration AS DWORD, ;
		dwNumTones AS DWORD, ;
		lpTones AS _WinLineGenerateTone) AS LONG PASCAL:tapi32.lineGenerateTone
_DLL FUNCTION lineGetAddressCaps( ;
		hLineApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		dwAddressID AS DWORD, ;
		dwAPIVersion AS DWORD, ;
		dwExtVersion AS DWORD, ;
		lpAddressCaps AS _WinLineAddressCaps) AS LONG PASCAL:tapi32.lineGetAddressCaps
_DLL FUNCTION lineGetAddressID( ;
		hLine AS DWORD PTR, ;
		lpdwAddressID AS DWORD PTR, ;
		dwAddressMode AS DWORD, ;
		lpsAddress AS PSZ, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.lineGetAddressID
_DLL FUNCTION lineGetAddressStatus( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		lpAddressStatus AS _WinLineAddressStatus) AS LONG PASCAL:tapi32.lineGetAddressStatus
_DLL FUNCTION lineGetCallInfo( ;
		hCall AS DWORD PTR, ;
		lpCallInfo AS _WinLineCallInfo) AS LONG PASCAL:tapi32.lineGetCallInfo
_DLL FUNCTION lineGetCallStatus( ;
		hCall AS DWORD PTR, ;
		lpCallInfo AS _WinLineCallStatus) AS LONG PASCAL:tapi32.lineGetCallStatus
_DLL FUNCTION lineGetConfRelatedCalls( ;
		hCall AS PTR, ;
		lpCallList AS _WinLineCallList) AS LONG PASCAL:tapi32.lineGetConfRelatedCalls
_DLL FUNCTION lineGetDevCaps( ;
		hLineApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		dwAPIVersion AS DWORD, ;
		dwExtVersion AS DWORD, ;
		lpLineDevCaps AS _WinLINEDEVCAPS) AS LONG PASCAL:tapi32.lineGetDevCaps
_DLL FUNCTION lineGetDevConfig( ;
		dwDeviceID AS DWORD, ;
		lpDeviceConfig AS _WinVarString, ;
		lpszDeviceClass AS PSZ) AS LONG PASCAL:tapi32.lineGetDevConfig
_DLL FUNCTION lineGetNewCalls( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		dwSelect AS DWORD, ;
		lpCallList AS _WinLineCallList) AS LONG PASCAL:tapi32.lineGetNewCalls
_DLL FUNCTION lineGetIcon( ;
		dwDeviceID AS DWORD, ;
		lpszDeviceClass AS PSZ, ;
		lphIcon AS DWORD PTR) AS LONG PASCAL:tapi32.lineGetIcon
_DLL FUNCTION lineGetID( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		hCall AS DWORD PTR, ;
		dwSelect AS DWORD, ;
		lpDeviceID AS _WinVarString, ;
		lpszDeviceclass AS PSZ) AS LONG PASCAL:tapi32.lineGetID
_DLL FUNCTION lineGetLineDevStatus( ;
		hLine AS PTR, ;
		lpLineDevStatus AS _WinLineDevStatus) AS LONG PASCAL:tapi32.lineGetLineDevStatus
_DLL FUNCTION lineGetRequest( ;
		hLineApp AS DWORD PTR, ;
		dwRequestMode AS DWORD, ;
		lpRequestBuffer AS PTR) AS LONG PASCAL:tapi32.lineGetRequest
_DLL FUNCTION lineGetStatusMessages( ;
		hLine AS DWORD PTR, ;
		lpdwLineStates AS DWORD PTR, ;
		lpdwAddressStates AS DWORD PTR) AS LONG PASCAL:tapi32.lineGetStatusMessages
_DLL FUNCTION lineHandoff( ;
		hCall AS DWORD PTR, ;
		lpszFileName AS PSZ, ;
		dwMediaMode AS DWORD) AS LONG PASCAL:tapi32.lineHandoff
_DLL FUNCTION lineHold( ;
		hCall AS DWORD PTR) AS LONG PASCAL:tapi32.lineHold
_DLL FUNCTION lineInitialize( ;
		lphLineApp AS DWORD PTR, ;
		hInstance AS DWORD PTR, ;
		lpfnCallback AS PTR, ;
		lpszAppName AS PSZ, ;
		lpdwNumDevs AS DWORD PTR) AS LONG PASCAL:tapi32.lineInitialize
_DLL FUNCTION lineMakeCall( ;
		hLine AS DWORD PTR, ;
		lphCall AS DWORD PTR, ;
		lpszDestAddress AS PSZ, ;
		dwCountryCode AS DWORD, ;
		lpCallParams AS _WinLineCallParams) AS LONG PASCAL:tapi32.lineMakeCall
_DLL FUNCTION lineMonitorDigits( ;
		hCall AS DWORD PTR, ;
		dwDigitModes AS DWORD) AS LONG PASCAL:tapi32.lineMonitorDigits
_DLL FUNCTION lineMonitorMedia( ;
		hCall AS DWORD PTR, ;
		dwMediaModes AS DWORD) AS LONG PASCAL:tapi32.lineMonitorMedia
_DLL FUNCTION lineMonitorTones( ;
		hCall AS DWORD PTR, ;
		lpToneList AS _WinLineMonitorTone, ;
		dwNumEntries AS DWORD) AS LONG PASCAL:tapi32.lineMonitorTones
_DLL FUNCTION lineNegotiateAPIVersion( ;
		hLineApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		dwAPILowVersion AS DWORD, ;
		dwAPIHighVersion AS DWORD, ;
		lpdwAPIVersion AS DWORD PTR, ;
		lpExtensionID AS _WinLineExtensionID) AS LONG PASCAL:tapi32.lineNegotiateAPIVersion
_DLL FUNCTION lineNegotiateExtVersion( ;
		hLineApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		dwAPIVersion AS DWORD, ;
		dwExtLowVersion AS DWORD, ;
		dwExtHighVersion AS DWORD, ;
		lpdwExtVersion AS DWORD PTR) AS LONG PASCAL:tapi32.lineNegotiateExtVersion
_DLL FUNCTION lineOpen( ;
		hLineApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		lphLine AS DWORD PTR, ;
		dwAPIVersion AS DWORD, ;
		dwExtVersion AS DWORD, ;
		dwCallbackInstance AS DWORD, ;
		dwPrivileges AS DWORD, ;
		dwMediaModes AS DWORD, ;
		lpCallParams AS _WinLineCallParams) AS LONG PASCAL:tapi32.lineOpen
_DLL FUNCTION linePark( ;
		hCall AS DWORD PTR, ;
		dwParkMode AS DWORD, ;
		lpszDirAddress AS PSZ, ;
		lpNonDirAddress AS _WinVarString) AS LONG PASCAL:tapi32.linePark
_DLL FUNCTION linePickup( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		lphCal AS DWORD PTR, ;
		lpszDestAddress AS PSZ, ;
		lpszGroupID AS PSZ) AS LONG PASCAL:tapi32.linePickup
_DLL FUNCTION linePrepareAddToConference( ;
		hCall AS DWORD PTR, ;
		lphConsultCall AS DWORD PTR, ;
		lpCallParams AS _WinLineCallParams) AS LONG PASCAL:tapi32.linePrepareAddToConference
_DLL FUNCTION lineRedirect( ;
		hCall AS DWORD PTR, ;
		lpszDestAddress AS PSZ, ;
		dwCountryCode AS DWORD) AS LONG PASCAL:tapi32.lineRedirect
_DLL FUNCTION lineRemoveFromConference( ;
		hCall AS DWORD PTR) AS LONG PASCAL:tapi32.lineRemoveFromConference
_DLL FUNCTION lineSecureCall( ;
		hCall AS DWORD PTR) AS LONG PASCAL:tapi32.lineSecureCall
_DLL FUNCTION lineSendUserUserInfo( ;
		hCall AS DWORD PTR, ;
		lpsUserUserInfo AS PSZ, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.lineSendUserUserInfo
_DLL FUNCTION lineSetAppSpecific( ;
		hCall AS DWORD PTR, ;
		dwAppSpecific AS DWORD) AS LONG PASCAL:tapi32.lineSetAppSpecific
_DLL FUNCTION lineSetCallParams( ;
		hCall AS DWORD PTR, ;
		dwBearerMode AS DWORD, ;
		dwMinRate AS DWORD, ;
		dwMaxRate AS DWORD, ;
		lpDialParams AS _WinLineDialParams) AS LONG PASCAL:tapi32.lineSetCallParams
_DLL FUNCTION lineSetCallPrivilege( ;
		hCall AS DWORD PTR, ;
		dwCallPrivilege AS DWORD) AS LONG PASCAL:tapi32.lineSetCallPrivilege
_DLL FUNCTION lineSetDevConfig( ;
		dwDeviceID AS DWORD, ;
		lpDeviceConfig AS PTR, ;
		dwSize AS DWORD, ;
		lpszDeviceClass AS PSZ) AS LONG PASCAL:tapi32.lineSetDevConfig
_DLL FUNCTION lineSetMediaControl( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		hCall AS DWORD PTR, ;
		dwSelect AS DWORD, ;
		lpDigitList AS _WinLineMediaControlDigit, ;
		dwDigitNumEntries AS DWORD, ;
		lpMediaList AS _WinLineMediaControlMedia, ;
		dwMediaNumEntries AS DWORD, ;
		lpToneList AS _WinLineMediaControlTone, ;
		dwToneNumEntries AS DWORD, ;
		lpCallStateList AS _WinLineMediaControlCallState, ;
		dwCallStateNumEntries AS DWORD) AS LONG PASCAL:tapi32.lineSetMediaControl
_DLL FUNCTION lineSetMediaMode( ;
		hCall AS DWORD PTR, ;
		dwMediaModes AS DWORD) AS LONG PASCAL:tapi32.lineSetMediaMode
_DLL FUNCTION lineSetNumRings( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		dwNumRings AS DWORD) AS LONG PASCAL:tapi32.lineSetNumRings
_DLL FUNCTION lineSetStatusMessages( ;
		hLine AS DWORD PTR, ;
		dwLineStates AS DWORD, ;
		dwAddressStates AS DWORD) AS LONG PASCAL:tapi32.lineSetStatusMessages
_DLL FUNCTION lineSetTerminal( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		hCall AS DWORD PTR, ;
		dwSelect AS DWORD, ;
		dwTerminalModes AS DWORD, ;
		dwTerminalID AS DWORD, ;
		bEnable AS DWORD) AS LONG PASCAL:tapi32.lineSetTerminal
_DLL FUNCTION lineSetTollList( ;
		hLineApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		lpszAddresIn AS PSZ, ;
		dwTollListOption AS DWORD) AS LONG PASCAL:tapi32.lineSetTollList
_DLL FUNCTION lineGetTranslateCaps( ;
		hLineApp AS DWORD PTR, ;
		dwAPIVersion AS DWORD, ;
		lpTranslateCaps AS _WinLineTranslateCaps) AS LONG PASCAL:tapi32.lineGetTranslateCaps
_DLL FUNCTION lineGetNumRings( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		lpdwNumRings AS DWORD PTR) AS LONG PASCAL:tapi32.lineGetNumRings
_DLL FUNCTION lineSetupConference( ;
		hCall AS DWORD PTR, ;
		hLine AS DWORD PTR, ;
		lphconfCall AS DWORD PTR, ;
		lphconsultCall AS DWORD PTR, ;
		dwNumParties AS DWORD, ;
		lpCallParams AS _WinLineCallParams) AS LONG PASCAL:tapi32.lineSetupConference
_DLL FUNCTION lineSetupTransfer( ;
		hCall AS DWORD PTR, ;
		lphconsultCall AS DWORD PTR, ;
		lpCallParams AS _WinLineCallParams) AS LONG PASCAL:tapi32.lineSetupTransfer
_DLL FUNCTION lineShutdown( ;
		hLineApp AS DWORD PTR) AS LONG PASCAL:tapi32.lineShutdown
_DLL FUNCTION lineSwapHold( ;
		hLine AS DWORD PTR, ;
		dwCompletionID AS DWORD) AS LONG PASCAL:tapi32.lineSwapHold
_DLL FUNCTION lineUncompleteCall( ;
		hLine AS DWORD PTR, ;
		dwCompletionID AS DWORD) AS LONG PASCAL:tapi32.lineUncompleteCall
_DLL FUNCTION lineUnhold( ;
		hCall AS DWORD PTR) AS LONG PASCAL:tapi32.lineUnhold
_DLL FUNCTION lineUnpark( ;
		hLine AS DWORD PTR, ;
		dwAddressID AS DWORD, ;
		lphCall AS DWORD PTR, ;
		lpszDestAddress AS PSZ) AS LONG PASCAL:tapi32.lineUnpark
_DLL FUNCTION lineReleaseUserUserInfo( ;
		hCall AS DWORD PTR) AS LONG PASCAL:tapi32.lineReleaseUserUserInfo
_DLL FUNCTION phoneClose( ;
		hPhone AS DWORD PTR) AS LONG PASCAL:tapi32.phoneClose
_DLL FUNCTION phoneConfigDialog( ;
		dwDeviceID AS DWORD, ;
		hwndOwner AS DWORD PTR, ;
		lpszDeviceClass AS PSZ) AS LONG PASCAL:tapi32.phoneConfigDialog
_DLL FUNCTION phoneDevSpecific( ;
		hPhone AS DWORD PTR, ;
		lpParams AS PTR, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.phoneDevSpecific
_DLL FUNCTION phoneGetButtonInfo( ;
		hPhone AS DWORD PTR, ;
		dwButtonLampID AS DWORD, ;
		lpButtonInfo AS _WinPhoneButtonInfo) AS LONG PASCAL:tapi32.phoneGetButtonInfo
_DLL FUNCTION phoneGetData( ;
		hPhone AS DWORD PTR, ;
		dwDataID AS DWORD, ;
		lpData AS PTR, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.phoneGetData
_DLL FUNCTION phoneGetDevCaps( ;
		hPhoneApp AS PTR, ;
		dwDeviceID AS DWORD, ;
		dwAPIVersion AS DWORD, ;
		dwExtVersion AS DWORD, ;
		lpPhoneCaps AS _WinPhoneCaps) AS LONG PASCAL:tapi32.phoneGetDevCaps
_DLL FUNCTION phoneGetDisplay( ;
		hPhone AS DWORD PTR, ;
		lpDisplay AS _WinVarString) AS LONG PASCAL:tapi32.phoneGetDisplay
_DLL FUNCTION phoneGetGain( ;
		hPhone AS DWORD PTR, ;
		dwHookSwitchDev AS DWORD, ;
		lpdwGain AS DWORD PTR) AS LONG PASCAL:tapi32.phoneGetGain
_DLL FUNCTION phoneGetHookSwitch( ;
		hPhone AS DWORD PTR, ;
		lpdwHoodSwitchDevs AS DWORD PTR) AS LONG PASCAL:tapi32.phoneGetHookSwitch
_DLL FUNCTION phoneGetIcon( ;
		dwDeviceID AS DWORD, ;
		lpszDeviceClass AS PSZ, ;
		lphIcon AS DWORD PTR) AS LONG PASCAL:tapi32.phoneGetIcon
_DLL FUNCTION phoneGetID( ;
		hPhone AS DWORD PTR, ;
		lpDeviceID AS PTR, ;
		lpszDeviceClass AS PSZ) AS LONG PASCAL:tapi32.phoneGetID
_DLL FUNCTION phoneGetLamp( ;
		hPhone AS DWORD PTR, ;
		dwButtonLampID AS DWORD, ;
		lpdwLampMode AS DWORD PTR) AS LONG PASCAL:tapi32.phoneGetLamp
_DLL FUNCTION phoneGetRing( ;
		hPhone AS DWORD PTR, ;
		lpdwRingMode AS DWORD PTR, ;
		lpdwVolume AS DWORD PTR) AS LONG PASCAL:tapi32.phoneGetRing
_DLL FUNCTION phoneGetStatus( ;
		hPhone AS DWORD PTR, ;
		lpPhoneStatus AS _WinPhoneStatus) AS LONG PASCAL:tapi32.phoneGetStatus
_DLL FUNCTION phoneGetStatusMessages( ;
		hPhone AS DWORD PTR, ;
		lpdwPhoneStates AS DWORD PTR, ;
		lpdwButtonModes AS DWORD PTR, ;
		lpdwButtonStates AS DWORD PTR) AS LONG PASCAL:tapi32.phoneGetStatusMessages
_DLL FUNCTION phoneGetVolume( ;
		hPhone AS DWORD PTR, ;
		dwHookSwitchDev AS DWORD, ;
		lpdwVolume AS DWORD PTR) AS LONG PASCAL:tapi32.phoneGetVolume
_DLL FUNCTION phoneInitialize( ;
		lphphone AS DWORD PTR, ;
		hInstance AS DWORD PTR, ;
		lpfnCallback AS PTR, ;
		lpszAppName AS PSZ, ;
		lpdwNumDevs AS DWORD PTR) AS LONG PASCAL:tapi32.phoneInitialize
_DLL FUNCTION phoneNegotiateAPIVersion( ;
		hPhoneApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		dwAPILowVersion AS DWORD, ;
		dwAPIHighVersion AS DWORD, ;
		lpdwAPIVersion AS DWORD PTR, ;
		lpExtensionID AS _WinPhoneExtensionID) AS LONG PASCAL:tapi32.phoneNegotiateAPIVersion
_DLL FUNCTION phoneNegotiateExtVersion( ;
		hPhoneApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		dwAPIVersion AS DWORD, ;
		dwAPIExtLowVersion AS DWORD, ;
		dwAPIExtHighVersion AS DWORD, ;
		lpdwExtVersion AS DWORD PTR) AS LONG PASCAL:tapi32.phoneNegotiateExtVersion
_DLL FUNCTION phoneOpen( ;
		hPhoneApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		lphPhone AS DWORD PTR, ;
		dwAPIVersion AS DWORD, ;
		dwExtVersion AS DWORD, ;
		dwCallbackInstance AS DWORD, ;
		dwPrivilege AS DWORD) AS LONG PASCAL:tapi32.phoneOpen
_DLL FUNCTION phoneSetButtonInfo( ;
		hPhone AS DWORD PTR, ;
		dwButtonLampID AS DWORD, ;
		lpButtonInfo AS _WinPhoneButtonInfo) AS LONG PASCAL:tapi32.phoneSetButtonInfo
_DLL FUNCTION phoneSetData( ;
		hPhone AS DWORD PTR, ;
		dwDataID AS DWORD, ;
		lpData AS PTR, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.phoneSetData
_DLL FUNCTION phoneSetDisplay( ;
		hPhone AS DWORD PTR, ;
		dwRow AS DWORD, ;
		dwColumn AS DWORD, ;
		lpsDisplay AS PSZ, ;
		dwSize AS DWORD) AS LONG PASCAL:tapi32.phoneSetDisplay
_DLL FUNCTION phoneSetGain( ;
		hPhone AS DWORD PTR, ;
		dwHookSwitchDev AS DWORD, ;
		dwGain AS DWORD) AS LONG PASCAL:tapi32.phoneSetGain
_DLL FUNCTION phoneSetHookSwitch( ;
		hPhone AS DWORD PTR, ;
		dwHookSwitchDevs AS DWORD, ;
		dwHookSwitchMode AS DWORD) AS LONG PASCAL:tapi32.phoneSetHookSwitch
_DLL FUNCTION phoneSetLamp( ;
		hPhone AS DWORD PTR, ;
		dwButtonLampID AS DWORD, ;
		dwLampMode AS DWORD) AS LONG PASCAL:tapi32.phoneSetLamp
_DLL FUNCTION phoneSetRing( ;
		hPhone AS DWORD PTR, ;
		dwRingMode AS DWORD, ;
		dwVolume AS DWORD) AS LONG PASCAL:tapi32.phoneSetRing
_DLL FUNCTION phoneSetStatusMessages( ;
		hPhone AS DWORD PTR, ;
		dwPhoneStates AS DWORD, ;
		dwButtonModes AS DWORD, ;
		dwButtonStates AS DWORD) AS LONG PASCAL:tapi32.phoneSetStatusMessages
_DLL FUNCTION phoneSetVolume( ;
		hPhone AS DWORD PTR, ;
		dwHookSwitchDev AS DWORD, ;
		dwVolume AS DWORD) AS LONG PASCAL:tapi32.phoneSetVolume
_DLL FUNCTION phoneShutdown( ;
		hPhoneApp AS DWORD PTR) AS LONG PASCAL:tapi32.phoneShutdown
_DLL FUNCTION lineTranslateDialog( ;
		hLineApp AS DWORD PTR, ;
		dwDeviceID AS DWORD, ;
		dwAPIVersion AS DWORD, ;
		hwndOwner AS DWORD PTR, ;
		lpszAddressIn AS PSZ) AS LONG PASCAL:tapi32.lineTranslateDialog
_DLL FUNCTION lineGetCountry( ;
		dwCountryID AS DWORD, ;
		dwAPIVersion AS DWORD, ;
		lpLinecountryList AS _WinLineCountryList) AS LONG PASCAL:tapi32.lineGetCountry
_DLL FUNCTION lineGetAppPriority( ;
		lpszAppFilename AS PSZ, ;
		dwMediaMode AS DWORD, ;
		lpExtensionID AS _WinLineExtensionID, ;
		dwRequestMode AS DWORD, ;
		lpExtensionName AS _WinVarString, ;
		lpdwPriority AS DWORD PTR) AS LONG PASCAL:tapi32.lineGetAppPriority
_DLL FUNCTION lineSetAppPriority( ;
		lpszAppFilename AS PSZ, ;
		dwMediaMode AS DWORD, ;
		lpExtensionID AS _WinLineExtensionID, ;
		dwRequestMode AS DWORD, ;
		lpszExtensionName AS PSZ, ;
		dwPriority AS DWORD) AS LONG PASCAL:tapi32.lineSetAppPriority
_DLL FUNCTION lineAddProvider( ;
		lpszProviderFilename AS PSZ, ;
		hwndOwner AS DWORD PTR, ;
		lpdwPermanentProviderID AS DWORD PTR) AS LONG PASCAL:tapi32.lineAddProvider
_DLL FUNCTION lineConfigProvider( ;
		hwndOwner AS DWORD PTR, ;
		dwPermanentProviderID AS DWORD) AS LONG PASCAL:tapi32.lineConfigProvider
_DLL FUNCTION lineRemoveProvider( ;
		dwPermanentProviderID AS DWORD, ;
		hwndOwner AS DWORD PTR) AS LONG PASCAL:tapi32.lineRemoveProvider
_DLL FUNCTION lineGetProviderList( ;
		dwAPIVersion AS DWORD, ;
		lpProviderList AS _WinLineProviderList) AS LONG PASCAL:tapi32.lineGetProviderList



#region defines
DEFINE LINE_ADDRESSSTATE           := 0L
DEFINE LINE_CALLINFO               := 1L
DEFINE LINE_CALLSTATE              := 2L
DEFINE LINE_CLOSE                  := 3L
DEFINE LINE_DEVSPECIFIC            := 4L
DEFINE LINE_DEVSPECIFICFEATURE	   := 5L
DEFINE LINE_GATHERDIGITS           := 6L
DEFINE LINE_GENERATE               := 7L
DEFINE LINE_LINEDEVSTATE           := 8L
DEFINE LINE_MONITORDIGITS          := 9L
DEFINE LINE_MONITORMEDIA           := 10L
DEFINE LINE_MONITORTONE            := 11L
DEFINE LINE_REPLY                  := 12L
DEFINE LINE_REQUEST                := 13L
DEFINE PHONE_BUTTON                := 14L
DEFINE PHONE_CLOSE                 := 15L
DEFINE PHONE_DEVSPECIFIC           := 16L
DEFINE PHONE_REPLY                 := 17L
DEFINE PHONE_STATE                 := 18L
DEFINE LINE_CREATE                 := 19L
DEFINE PHONE_CREATE                := 20L
	// Define Simple Telephony Constants.
DEFINE TAPI_REPLY                    := WM_USER + 99
DEFINE TAPIERR_CONNECTED           := 0L
DEFINE TAPIERR_DROPPED             := -1L
DEFINE TAPIERR_NOREQUESTRECIPIENT  := -2L
DEFINE TAPIERR_REQUESTQUEUEFULL    := -3L
DEFINE TAPIERR_INVALDESTADDRESS    := -4L
DEFINE TAPIERR_INVALWINDOWHANDLE   := -5L
DEFINE TAPIERR_INVALDEVICECLASS    := -6L
DEFINE TAPIERR_INVALDEVICEID       := -7L
DEFINE TAPIERR_DEVICECLASSUNAVAIL  := -8L
DEFINE TAPIERR_DEVICEIDUNAVAIL     := -9L
DEFINE TAPIERR_DEVICEINUSE         := -10L
DEFINE TAPIERR_DESTBUSY            := -11L
DEFINE TAPIERR_DESTNOANSWER        := -12L
DEFINE TAPIERR_DESTUNAVAIL         := -13L
DEFINE TAPIERR_UNKNOWNWINHANDLE    := -14L
DEFINE TAPIERR_UNKNOWNREQUESTID    := -15L
DEFINE TAPIERR_REQUESTFAILED       := -16L
DEFINE TAPIERR_REQUESTCANCELLED    := -17L
DEFINE TAPIERR_INVALPOINTER        := -18L
DEFINE TAPIMAXDESTADDRESSSIZE    := 80L
DEFINE TAPIMAXAPPNAMESIZE        := 40L
DEFINE TAPIMAXCALLEDPARTYSIZE    := 40L
DEFINE TAPIMAXCOMMENTSIZE        := 80L
DEFINE TAPIMAXDEVICECLASSSIZE    := 40L
DEFINE TAPIMAXDEVICEIDSIZE       := 40L
	// Data types and values for Phones
DEFINE PHONEBUTTONFUNCTION_UNKNOWN          := 0x00000000
DEFINE PHONEBUTTONFUNCTION_CONFERENCE       := 0x00000001
DEFINE PHONEBUTTONFUNCTION_TRANSFER         := 0x00000002
DEFINE PHONEBUTTONFUNCTION_DROP             := 0x00000003
DEFINE PHONEBUTTONFUNCTION_HOLD             := 0x00000004
DEFINE PHONEBUTTONFUNCTION_RECALL           := 0x00000005
DEFINE PHONEBUTTONFUNCTION_DISCONNECT       := 0x00000006
DEFINE PHONEBUTTONFUNCTION_CONNECT          := 0x00000007
DEFINE PHONEBUTTONFUNCTION_MSGWAITON        := 0x00000008
DEFINE PHONEBUTTONFUNCTION_MSGWAITOFF       := 0x00000009
DEFINE PHONEBUTTONFUNCTION_SELECTRING       := 0x0000000A
DEFINE PHONEBUTTONFUNCTION_ABBREVDIAL       := 0x0000000B
DEFINE PHONEBUTTONFUNCTION_FORWARD          := 0x0000000C
DEFINE PHONEBUTTONFUNCTION_PICKUP           := 0x0000000D
DEFINE PHONEBUTTONFUNCTION_RINGAGAIN        := 0x0000000E
DEFINE PHONEBUTTONFUNCTION_PARK             := 0x0000000F
DEFINE PHONEBUTTONFUNCTION_REJECT           := 0x00000010
DEFINE PHONEBUTTONFUNCTION_REDIRECT         := 0x00000011
DEFINE PHONEBUTTONFUNCTION_MUTE             := 0x00000012
DEFINE PHONEBUTTONFUNCTION_VOLUMEUP         := 0x00000013
DEFINE PHONEBUTTONFUNCTION_VOLUMEDOWN       := 0x00000014
DEFINE PHONEBUTTONFUNCTION_SPEAKERON        := 0x00000015
DEFINE PHONEBUTTONFUNCTION_SPEAKEROFF       := 0x00000016
DEFINE PHONEBUTTONFUNCTION_FLASH            := 0x00000017
DEFINE PHONEBUTTONFUNCTION_DATAON           := 0x00000018
DEFINE PHONEBUTTONFUNCTION_DATAOFF          := 0x00000019
DEFINE PHONEBUTTONFUNCTION_DONOTDISTURB     := 0x0000001A
DEFINE PHONEBUTTONFUNCTION_INTERCOM         := 0x0000001B
DEFINE PHONEBUTTONFUNCTION_BRIDGEDAPP       := 0x0000001C
DEFINE PHONEBUTTONFUNCTION_BUSY             := 0x0000001D
DEFINE PHONEBUTTONFUNCTION_CALLAPP          := 0x0000001E
DEFINE PHONEBUTTONFUNCTION_DATETIME         := 0x0000001F
DEFINE PHONEBUTTONFUNCTION_DIRECTORY        := 0x00000020
DEFINE PHONEBUTTONFUNCTION_COVER            := 0x00000021
DEFINE PHONEBUTTONFUNCTION_CALLID           := 0x00000022
DEFINE PHONEBUTTONFUNCTION_LASTNUM          := 0x00000023
DEFINE PHONEBUTTONFUNCTION_NIGHTSRV         := 0x00000024
DEFINE PHONEBUTTONFUNCTION_SENDCALLS        := 0x00000025
DEFINE PHONEBUTTONFUNCTION_MSGINDICATOR     := 0x00000026
DEFINE PHONEBUTTONFUNCTION_REPDIAL          := 0x00000027
DEFINE PHONEBUTTONFUNCTION_SETREPDIAL       := 0x00000028
DEFINE PHONEBUTTONFUNCTION_SYSTEMSPEED      := 0x00000029
DEFINE PHONEBUTTONFUNCTION_STATIONSPEED     := 0x0000002A
DEFINE PHONEBUTTONFUNCTION_CAMPON           := 0x0000002B
DEFINE PHONEBUTTONFUNCTION_SAVEREPEAT       := 0x0000002C
DEFINE PHONEBUTTONFUNCTION_QUEUECALL        := 0x0000002D
DEFINE PHONEBUTTONFUNCTION_NONE             := 0x0000002E
DEFINE PHONEBUTTONMODE_DUMMY        := 0x00000001
DEFINE PHONEBUTTONMODE_CALL         := 0x00000002
DEFINE PHONEBUTTONMODE_FEATURE      := 0x00000004
DEFINE PHONEBUTTONMODE_KEYPAD       := 0x00000008
DEFINE PHONEBUTTONMODE_LOCAL        := 0x00000010
DEFINE PHONEBUTTONMODE_DISPLAY      := 0x00000020
DEFINE PHONEBUTTONSTATE_UP          := 0x00000001
DEFINE PHONEBUTTONSTATE_DOWN        := 0x00000002
DEFINE PHONEBUTTONSTATE_UNKNOWN     := 0x00000004
DEFINE PHONEBUTTONSTATE_UNAVAIL     := 0x00000008
DEFINE PHONEERR_ALLOCATED              := 0x90000001
DEFINE PHONEERR_BADDEVICEID            := 0x90000002
DEFINE PHONEERR_INCOMPATIBLEAPIVERSION := 0x90000003
DEFINE PHONEERR_INCOMPATIBLEEXTVERSION := 0x90000004
DEFINE PHONEERR_INIFILECORRUPT         := 0x90000005
DEFINE PHONEERR_INUSE                  := 0x90000006
DEFINE PHONEERR_INVALAPPHANDLE         := 0x90000007
DEFINE PHONEERR_INVALAPPNAME           := 0x90000008
DEFINE PHONEERR_INVALBUTTONLAMPID      := 0x90000009
DEFINE PHONEERR_INVALBUTTONMODE        := 0x9000000A
DEFINE PHONEERR_INVALBUTTONSTATE       := 0x9000000B
DEFINE PHONEERR_INVALDATAID            := 0x9000000C
DEFINE PHONEERR_INVALDEVICECLASS       := 0x9000000D
DEFINE PHONEERR_INVALEXTVERSION        := 0x9000000E
DEFINE PHONEERR_INVALHOOKSWITCHDEV     := 0x9000000F
DEFINE PHONEERR_INVALHOOKSWITCHMODE    := 0x90000010
DEFINE PHONEERR_INVALLAMPMODE          := 0x90000011
DEFINE PHONEERR_INVALPARAM             := 0x90000012
DEFINE PHONEERR_INVALPHONEHANDLE       := 0x90000013
DEFINE PHONEERR_INVALPHONESTATE        := 0x90000014
DEFINE PHONEERR_INVALPOINTER           := 0x90000015
DEFINE PHONEERR_INVALPRIVILEGE         := 0x90000016
DEFINE PHONEERR_INVALRINGMODE          := 0x90000017
DEFINE PHONEERR_NODEVICE               := 0x90000018
DEFINE PHONEERR_NODRIVER               := 0x90000019
DEFINE PHONEERR_NOMEM                  := 0x9000001A
DEFINE PHONEERR_NOTOWNER               := 0x9000001B
DEFINE PHONEERR_OPERATIONFAILED        := 0x9000001C
DEFINE PHONEERR_OPERATIONUNAVAIL       := 0x9000001D
DEFINE PHONEERR_RESOURCEUNAVAIL        := 0x9000001F
DEFINE PHONEERR_REQUESTOVERRUN         := 0x90000020
DEFINE PHONEERR_STRUCTURETOOSMALL      := 0x90000021
DEFINE PHONEERR_UNINITIALIZED          := 0x90000022
DEFINE PHONEERR_REINIT                 := 0x90000023
DEFINE PHONEHOOKSWITCHDEV_HANDSET      := 0x00000001
DEFINE PHONEHOOKSWITCHDEV_SPEAKER      := 0x00000002
DEFINE PHONEHOOKSWITCHDEV_HEADSET      := 0x00000004
DEFINE PHONEHOOKSWITCHMODE_ONHOOK      := 0x00000001
DEFINE PHONEHOOKSWITCHMODE_MIC         := 0x00000002
DEFINE PHONEHOOKSWITCHMODE_SPEAKER     := 0x00000004
DEFINE PHONEHOOKSWITCHMODE_MICSPEAKER  := 0x00000008
DEFINE PHONEHOOKSWITCHMODE_UNKNOWN     := 0x00000010
DEFINE PHONELAMPMODE_DUMMY              := 0x00000001
DEFINE PHONELAMPMODE_OFF                := 0x00000002
DEFINE PHONELAMPMODE_STEADY             := 0x00000004
DEFINE PHONELAMPMODE_WINK               := 0x00000008
DEFINE PHONELAMPMODE_FLASH              := 0x00000010
DEFINE PHONELAMPMODE_FLUTTER            := 0x00000020
DEFINE PHONELAMPMODE_BROKENFLUTTER      := 0x00000040
DEFINE PHONELAMPMODE_UNKNOWN            := 0x00000080
DEFINE PHONEPRIVILEGE_MONITOR        := 0x00000001
DEFINE PHONEPRIVILEGE_OWNER          := 0x00000002
DEFINE PHONESTATE_OTHER               := 0x00000001
DEFINE PHONESTATE_CONNECTED           := 0x00000002
DEFINE PHONESTATE_DISCONNECTED        := 0x00000004
DEFINE PHONESTATE_OWNER               := 0x00000008
DEFINE PHONESTATE_MONITORS            := 0x00000010
DEFINE PHONESTATE_DISPLAY             := 0x00000020
DEFINE PHONESTATE_LAMP                := 0x00000040
DEFINE PHONESTATE_RINGMODE            := 0x00000080
DEFINE PHONESTATE_RINGVOLUME          := 0x00000100
DEFINE PHONESTATE_HANDSETHOOKSWITCH   := 0x00000200
DEFINE PHONESTATE_HANDSETVOLUME       := 0x00000400
DEFINE PHONESTATE_HANDSETGAIN         := 0x00000800
DEFINE PHONESTATE_SPEAKERHOOKSWITCH   := 0x00001000
DEFINE PHONESTATE_SPEAKERVOLUME       := 0x00002000
DEFINE PHONESTATE_SPEAKERGAIN         := 0x00004000
DEFINE PHONESTATE_HEADSETHOOKSWITCH   := 0x00008000
DEFINE PHONESTATE_HEADSETVOLUME       := 0x00010000
DEFINE PHONESTATE_HEADSETGAIN         := 0x00020000
DEFINE PHONESTATE_SUSPEND             := 0x00040000
DEFINE PHONESTATE_RESUME              := 0x00080000
DEFINE PHONESTATE_DEVSPECIFIC         := 0x00100000
DEFINE PHONESTATE_REINIT              := 0x00200000
DEFINE PHONESTATE_CAPSCHANGE          := 0x00400000
DEFINE PHONESTATE_REMOVED             := 0x00800000
DEFINE PHONESTATUSFLAGS_CONNECTED    := 0x00000001
DEFINE PHONESTATUSFLAGS_SUSPENDED    := 0x00000002
DEFINE STRINGFORMAT_ASCII   := 0x00000001
DEFINE STRINGFORMAT_DBCS    := 0x00000002
DEFINE STRINGFORMAT_UNICODE := 0x00000003
DEFINE STRINGFORMAT_BINARY  := 0x00000004
	// Data types and values for Lines
DEFINE LINEADDRCAPFLAGS_FWDNUMRINGS        := 0x00000001
DEFINE LINEADDRCAPFLAGS_PICKUPGROUPID      := 0x00000002
DEFINE LINEADDRCAPFLAGS_SECURE             := 0x00000004
DEFINE LINEADDRCAPFLAGS_BLOCKIDDEFAULT     := 0x00000008
DEFINE LINEADDRCAPFLAGS_BLOCKIDOVERRIDE    := 0x00000010
DEFINE LINEADDRCAPFLAGS_DIALED             := 0x00000020
DEFINE LINEADDRCAPFLAGS_ORIGOFFHOOK        := 0x00000040
DEFINE LINEADDRCAPFLAGS_DESTOFFHOOK        := 0x00000080
DEFINE LINEADDRCAPFLAGS_FWDCONSULT         := 0x00000100
DEFINE LINEADDRCAPFLAGS_SETUPCONFNULL      := 0x00000200
DEFINE LINEADDRCAPFLAGS_AUTORECONNECT      := 0x00000400
DEFINE LINEADDRCAPFLAGS_COMPLETIONID       := 0x00000800
DEFINE LINEADDRCAPFLAGS_TRANSFERHELD       := 0x00001000
DEFINE LINEADDRCAPFLAGS_TRANSFERMAKE       := 0x00002000
DEFINE LINEADDRCAPFLAGS_CONFERENCEHELD     := 0x00004000
DEFINE LINEADDRCAPFLAGS_CONFERENCEMAKE     := 0x00008000
DEFINE LINEADDRCAPFLAGS_PARTIALDIAL        := 0x00010000
DEFINE LINEADDRCAPFLAGS_FWDSTATUSVALID     := 0x00020000
DEFINE LINEADDRCAPFLAGS_FWDINTEXTADDR      := 0x00040000
DEFINE LINEADDRCAPFLAGS_FWDBUSYNAADDR      := 0x00080000
DEFINE LINEADDRCAPFLAGS_ACCEPTTOALERT      := 0x00100000
DEFINE LINEADDRCAPFLAGS_CONFDROP           := 0x00200000
DEFINE LINEADDRCAPFLAGS_PICKUPCALLWAIT     := 0x00400000
DEFINE LINEADDRESSMODE_ADDRESSID        := 0x00000001
DEFINE LINEADDRESSMODE_DIALABLEADDR     := 0x00000002
DEFINE LINEADDRESSSHARING_PRIVATE        := 0x00000001
DEFINE LINEADDRESSSHARING_BRIDGEDEXCL    := 0x00000002
DEFINE LINEADDRESSSHARING_BRIDGEDNEW     := 0x00000004
DEFINE LINEADDRESSSHARING_BRIDGEDSHARED  := 0x00000008
DEFINE LINEADDRESSSHARING_MONITORED      := 0x00000010
DEFINE LINEADDRESSSTATE_OTHER            := 0x00000001
DEFINE LINEADDRESSSTATE_DEVSPECIFIC      := 0x00000002
DEFINE LINEADDRESSSTATE_INUSEZERO        := 0x00000004
DEFINE LINEADDRESSSTATE_INUSEONE         := 0x00000008
DEFINE LINEADDRESSSTATE_INUSEMANY        := 0x00000010
DEFINE LINEADDRESSSTATE_NUMCALLS         := 0x00000020
DEFINE LINEADDRESSSTATE_FORWARD          := 0x00000040
DEFINE LINEADDRESSSTATE_TERMINALS        := 0x00000080
DEFINE LINEADDRESSSTATE_CAPSCHANGE         := 0x00000100
DEFINE LINEADDRFEATURE_FORWARD           := 0x00000001
DEFINE LINEADDRFEATURE_MAKECALL          := 0x00000002
DEFINE LINEADDRFEATURE_PICKUP            := 0x00000004
DEFINE LINEADDRFEATURE_SETMEDIACONTROL   := 0x00000008
DEFINE LINEADDRFEATURE_SETTERMINAL       := 0x00000010
DEFINE LINEADDRFEATURE_SETUPCONF         := 0x00000020
DEFINE LINEADDRFEATURE_UNCOMPLETECALL    := 0x00000040
DEFINE LINEADDRFEATURE_UNPARK            := 0x00000080
DEFINE LINEANSWERMODE_NONE    := 0x00000001
DEFINE LINEANSWERMODE_DROP    := 0x00000002
DEFINE LINEANSWERMODE_HOLD    := 0x00000004
DEFINE LINEBEARERMODE_VOICE            := 0x00000001
DEFINE LINEBEARERMODE_SPEECH           := 0x00000002
DEFINE LINEBEARERMODE_MULTIUSE         := 0x00000004
DEFINE LINEBEARERMODE_DATA             := 0x00000008
DEFINE LINEBEARERMODE_ALTSPEECHDATA    := 0x00000010
DEFINE LINEBEARERMODE_NONCALLSIGNALING := 0x00000020
DEFINE LINEBEARERMODE_PASSTHROUGH      := 0x00000040
DEFINE LINEBUSYMODE_STATION    := 0x00000001
DEFINE LINEBUSYMODE_TRUNK      := 0x00000002
DEFINE LINEBUSYMODE_UNKNOWN    := 0x00000004
DEFINE LINEBUSYMODE_UNAVAIL    := 0x00000008
DEFINE LINECALLCOMPLCOND_BUSY        := 0x00000001
DEFINE LINECALLCOMPLCOND_NOANSWER    := 0x00000002
DEFINE LINECALLCOMPLMODE_CAMPON      := 0x00000001
DEFINE LINECALLCOMPLMODE_CALLBACK    := 0x00000002
DEFINE LINECALLCOMPLMODE_INTRUDE     := 0x00000004
DEFINE LINECALLCOMPLMODE_MESSAGE     := 0x00000008
DEFINE LINECALLFEATURE_ACCEPT                   := 0x00000001
DEFINE LINECALLFEATURE_ADDTOCONF                := 0x00000002
DEFINE LINECALLFEATURE_ANSWER                   := 0x00000004
DEFINE LINECALLFEATURE_BLINDTRANSFER            := 0x00000008
DEFINE LINECALLFEATURE_COMPLETECALL             := 0x00000010
DEFINE LINECALLFEATURE_COMPLETETRANSF           := 0x00000020
DEFINE LINECALLFEATURE_DIAL                     := 0x00000040
DEFINE LINECALLFEATURE_DROP                     := 0x00000080
DEFINE LINECALLFEATURE_GATHERDIGITS             := 0x00000100
DEFINE LINECALLFEATURE_GENERATEDIGITS           := 0x00000200
DEFINE LINECALLFEATURE_GENERATETONE             := 0x00000400
DEFINE LINECALLFEATURE_HOLD                     := 0x00000800
DEFINE LINECALLFEATURE_MONITORDIGITS            := 0x00001000
DEFINE LINECALLFEATURE_MONITORMEDIA             := 0x00002000
DEFINE LINECALLFEATURE_MONITORTONES             := 0x00004000
DEFINE LINECALLFEATURE_PARK                     := 0x00008000
DEFINE LINECALLFEATURE_PREPAREADDCONF           := 0x00010000
DEFINE LINECALLFEATURE_REDIRECT                 := 0x00020000
DEFINE LINECALLFEATURE_REMOVEFROMCONF           := 0x00040000
DEFINE LINECALLFEATURE_SECURECALL               := 0x00080000
DEFINE LINECALLFEATURE_SENDUSERUSER             := 0x00100000
DEFINE LINECALLFEATURE_SETCALLPARAMS            := 0x00200000
DEFINE LINECALLFEATURE_SETMEDIACONTROL          := 0x00400000
DEFINE LINECALLFEATURE_SETTERMINAL              := 0x00800000
DEFINE LINECALLFEATURE_SETUPCONF                := 0x01000000
DEFINE LINECALLFEATURE_SETUPTRANSFER            := 0x02000000
DEFINE LINECALLFEATURE_SWAPHOLD                 := 0x04000000
DEFINE LINECALLFEATURE_UNHOLD                   := 0x08000000
DEFINE LINECALLFEATURE_RELEASEUSERUSERINFO      := 0x10000000
DEFINE LINECALLINFOSTATE_OTHER             := 0x00000001
DEFINE LINECALLINFOSTATE_DEVSPECIFIC       := 0x00000002
DEFINE LINECALLINFOSTATE_BEARERMODE        := 0x00000004
DEFINE LINECALLINFOSTATE_RATE              := 0x00000008
DEFINE LINECALLINFOSTATE_MEDIAMODE         := 0x00000010
DEFINE LINECALLINFOSTATE_APPSPECIFIC       := 0x00000020
DEFINE LINECALLINFOSTATE_CALLID            := 0x00000040
DEFINE LINECALLINFOSTATE_RELATEDCALLID     := 0x00000080
DEFINE LINECALLINFOSTATE_ORIGIN            := 0x00000100
DEFINE LINECALLINFOSTATE_REASON            := 0x00000200
DEFINE LINECALLINFOSTATE_COMPLETIONID      := 0x00000400
DEFINE LINECALLINFOSTATE_NUMOWNERINCR      := 0x00000800
DEFINE LINECALLINFOSTATE_NUMOWNERDECR      := 0x00001000
DEFINE LINECALLINFOSTATE_NUMMONITORS       := 0x00002000
DEFINE LINECALLINFOSTATE_TRUNK             := 0x00004000
DEFINE LINECALLINFOSTATE_CALLERID          := 0x00008000
DEFINE LINECALLINFOSTATE_CALLEDID          := 0x00010000
DEFINE LINECALLINFOSTATE_CONNECTEDID       := 0x00020000
DEFINE LINECALLINFOSTATE_REDIRECTIONID     := 0x00040000
DEFINE LINECALLINFOSTATE_REDIRECTINGID     := 0x00080000
DEFINE LINECALLINFOSTATE_DISPLAY           := 0x00100000
DEFINE LINECALLINFOSTATE_USERUSERINFO      := 0x00200000
DEFINE LINECALLINFOSTATE_HIGHLEVELCOMP     := 0x00400000
DEFINE LINECALLINFOSTATE_LOWLEVELCOMP      := 0x00800000
DEFINE LINECALLINFOSTATE_CHARGINGINFO      := 0x01000000
DEFINE LINECALLINFOSTATE_TERMINAL          := 0x02000000
DEFINE LINECALLINFOSTATE_DIALPARAMS        := 0x04000000
DEFINE LINECALLINFOSTATE_MONITORMODES      := 0x08000000
DEFINE LINECALLORIGIN_OUTBOUND    := 0x00000001
DEFINE LINECALLORIGIN_INTERNAL    := 0x00000002
DEFINE LINECALLORIGIN_EXTERNAL    := 0x00000004
DEFINE LINECALLORIGIN_UNKNOWN     := 0x00000010
DEFINE LINECALLORIGIN_UNAVAIL     := 0x00000020
DEFINE LINECALLORIGIN_CONFERENCE  := 0x00000040
DEFINE LINECALLORIGIN_INBOUND     := 0x00000080
DEFINE LINECALLPARAMFLAGS_SECURE           := 0x00000001
DEFINE LINECALLPARAMFLAGS_IDLE             := 0x00000002
DEFINE LINECALLPARAMFLAGS_BLOCKID          := 0x00000004
DEFINE LINECALLPARAMFLAGS_ORIGOFFHOOK      := 0x00000008
DEFINE LINECALLPARAMFLAGS_DESTOFFHOOK      := 0x00000010
DEFINE LINECALLPARTYID_BLOCKED         := 0x00000001
DEFINE LINECALLPARTYID_OUTOFAREA       := 0x00000002
DEFINE LINECALLPARTYID_NAME            := 0x00000004
DEFINE LINECALLPARTYID_ADDRESS         := 0x00000008
DEFINE LINECALLPARTYID_PARTIAL         := 0x00000010
DEFINE LINECALLPARTYID_UNKNOWN         := 0x00000020
DEFINE LINECALLPARTYID_UNAVAIL         := 0x00000040
DEFINE LINECALLPRIVILEGE_NONE          := 0x00000001
DEFINE LINECALLPRIVILEGE_MONITOR       := 0x00000002
DEFINE LINECALLPRIVILEGE_OWNER         := 0x00000004
DEFINE LINECALLREASON_DIRECT           := 0x00000001
DEFINE LINECALLREASON_FWDBUSY          := 0x00000002
DEFINE LINECALLREASON_FWDNOANSWER      := 0x00000004
DEFINE LINECALLREASON_FWDUNCOND        := 0x00000008
DEFINE LINECALLREASON_PICKUP           := 0x00000010
DEFINE LINECALLREASON_UNPARK           := 0x00000020
DEFINE LINECALLREASON_REDIRECT         := 0x00000040
DEFINE LINECALLREASON_CALLCOMPLETION   := 0x00000080
DEFINE LINECALLREASON_TRANSFER         := 0x00000100
DEFINE LINECALLREASON_REMINDER         := 0x00000200
DEFINE LINECALLREASON_UNKNOWN          := 0x00000400
DEFINE LINECALLREASON_UNAVAIL          := 0x00000800
DEFINE LINECALLREASON_INTRUDE          := 0x00001000
DEFINE LINECALLREASON_PARKED           := 0x00002000
DEFINE LINECALLSELECT_LINE     := 0x00000001
DEFINE LINECALLSELECT_ADDRESS  := 0x00000002
DEFINE LINECALLSELECT_CALL     := 0x00000004
DEFINE LINECALLSTATE_IDLE                  := 0x00000001
DEFINE LINECALLSTATE_OFFERING              := 0x00000002
DEFINE LINECALLSTATE_ACCEPTED              := 0x00000004
DEFINE LINECALLSTATE_DIALTONE              := 0x00000008
DEFINE LINECALLSTATE_DIALING               := 0x00000010
DEFINE LINECALLSTATE_RINGBACK              := 0x00000020
DEFINE LINECALLSTATE_BUSY                  := 0x00000040
DEFINE LINECALLSTATE_SPECIALINFO           := 0x00000080
DEFINE LINECALLSTATE_CONNECTED             := 0x00000100
DEFINE LINECALLSTATE_PROCEEDING            := 0x00000200
DEFINE LINECALLSTATE_ONHOLD                := 0x00000400
DEFINE LINECALLSTATE_CONFERENCED           := 0x00000800
DEFINE LINECALLSTATE_ONHOLDPENDCONF        := 0x00001000
DEFINE LINECALLSTATE_ONHOLDPENDTRANSFER    := 0x00002000
DEFINE LINECALLSTATE_DISCONNECTED          := 0x00004000
DEFINE LINECALLSTATE_UNKNOWN               := 0x00008000
DEFINE LINECONNECTEDMODE_ACTIVE            := 0x00000001
DEFINE LINECONNECTEDMODE_INACTIVE          := 0x00000002
DEFINE LINEOFFERINGMODE_ACTIVE             := 0x00000001
DEFINE LINEOFFERINGMODE_INACTIVE           := 0x00000002
DEFINE LINEDEVCAPFLAGS_CROSSADDRCONF   := 0x00000001
DEFINE LINEDEVCAPFLAGS_HIGHLEVCOMP     := 0x00000002
DEFINE LINEDEVCAPFLAGS_LOWLEVCOMP      := 0x00000004
DEFINE LINEDEVCAPFLAGS_MEDIACONTROL    := 0x00000008
DEFINE LINEDEVCAPFLAGS_MULTIPLEADDR    := 0x00000010
DEFINE LINEDEVCAPFLAGS_CLOSEDROP       := 0x00000020
DEFINE LINEDEVCAPFLAGS_DIALBILLING     := 0x00000040
DEFINE LINEDEVCAPFLAGS_DIALQUIET       := 0x00000080
DEFINE LINEDEVCAPFLAGS_DIALDIALTONE    := 0x00000100
DEFINE LINEDEVSTATE_OTHER              := 0x00000001
DEFINE LINEDEVSTATE_RINGING            := 0x00000002
DEFINE LINEDEVSTATE_CONNECTED          := 0x00000004
DEFINE LINEDEVSTATE_DISCONNECTED       := 0x00000008
DEFINE LINEDEVSTATE_MSGWAITON          := 0x00000010
DEFINE LINEDEVSTATE_MSGWAITOFF         := 0x00000020
DEFINE LINEDEVSTATE_INSERVICE          := 0x00000040
DEFINE LINEDEVSTATE_OUTOFSERVICE       := 0x00000080
DEFINE LINEDEVSTATE_MAINTENANCE        := 0x00000100
DEFINE LINEDEVSTATE_OPEN               := 0x00000200
DEFINE LINEDEVSTATE_CLOSE              := 0x00000400
DEFINE LINEDEVSTATE_NUMCALLS           := 0x00000800
DEFINE LINEDEVSTATE_NUMCOMPLETIONS     := 0x00001000
DEFINE LINEDEVSTATE_TERMINALS          := 0x00002000
DEFINE LINEDEVSTATE_ROAMMODE           := 0x00004000
DEFINE LINEDEVSTATE_BATTERY            := 0x00008000
DEFINE LINEDEVSTATE_SIGNAL             := 0x00010000
DEFINE LINEDEVSTATE_DEVSPECIFIC        := 0x00020000
DEFINE LINEDEVSTATE_REINIT             := 0x00040000
DEFINE LINEDEVSTATE_LOCK               := 0x00080000
DEFINE LINEDEVSTATE_CAPSCHANGE         := 0x00100000
DEFINE LINEDEVSTATE_CONFIGCHANGE       := 0x00200000
DEFINE LINEDEVSTATE_TRANSLATECHANGE    := 0x00400000
DEFINE LINEDEVSTATE_COMPLCANCEL        := 0x00800000
DEFINE LINEDEVSTATE_REMOVED            := 0x01000000
DEFINE LINEDEVSTATUSFLAGS_CONNECTED  := 0x00000001
DEFINE LINEDEVSTATUSFLAGS_MSGWAIT    := 0x00000002
DEFINE LINEDEVSTATUSFLAGS_INSERVICE  := 0x00000004
DEFINE LINEDEVSTATUSFLAGS_LOCKED     := 0x00000008
DEFINE LINEDIALTONEMODE_NORMAL    := 0x00000001
DEFINE LINEDIALTONEMODE_SPECIAL   := 0x00000002
DEFINE LINEDIALTONEMODE_INTERNAL  := 0x00000004
DEFINE LINEDIALTONEMODE_EXTERNAL  := 0x00000008
DEFINE LINEDIALTONEMODE_UNKNOWN   := 0x00000010
DEFINE LINEDIALTONEMODE_UNAVAIL   := 0x00000020
DEFINE LINEDIGITMODE_PULSE    := 0x00000001
DEFINE LINEDIGITMODE_DTMF     := 0x00000002
DEFINE LINEDIGITMODE_DTMFEND  := 0x00000004
DEFINE LINEDISCONNECTMODE_NORMAL       := 0x00000001
DEFINE LINEDISCONNECTMODE_UNKNOWN      := 0x00000002
DEFINE LINEDISCONNECTMODE_REJECT       := 0x00000004
DEFINE LINEDISCONNECTMODE_PICKUP       := 0x00000008
DEFINE LINEDISCONNECTMODE_FORWARDED    := 0x00000010
DEFINE LINEDISCONNECTMODE_BUSY         := 0x00000020
DEFINE LINEDISCONNECTMODE_NOANSWER     := 0x00000040
DEFINE LINEDISCONNECTMODE_BADADDRESS   := 0x00000080
DEFINE LINEDISCONNECTMODE_UNREACHABLE  := 0x00000100
DEFINE LINEDISCONNECTMODE_CONGESTION   := 0x00000200
DEFINE LINEDISCONNECTMODE_INCOMPATIBLE := 0x00000400
DEFINE LINEDISCONNECTMODE_UNAVAIL      := 0x00000800
DEFINE LINEDISCONNECTMODE_NODIALTONE   := 0x00001000
DEFINE LINEERR_ALLOCATED                   := 0x80000001
DEFINE LINEERR_BADDEVICEID                 := 0x80000002
DEFINE LINEERR_BEARERMODEUNAVAIL           := 0x80000003
DEFINE LINEERR_CALLUNAVAIL                 := 0x80000005
DEFINE LINEERR_COMPLETIONOVERRUN           := 0x80000006
DEFINE LINEERR_CONFERENCEFULL              := 0x80000007
DEFINE LINEERR_DIALBILLING                 := 0x80000008
DEFINE LINEERR_DIALDIALTONE                := 0x80000009
DEFINE LINEERR_DIALPROMPT                  := 0x8000000A
DEFINE LINEERR_DIALQUIET                   := 0x8000000B
DEFINE LINEERR_INCOMPATIBLEAPIVERSION      := 0x8000000C
DEFINE LINEERR_INCOMPATIBLEEXTVERSION      := 0x8000000D
DEFINE LINEERR_INIFILECORRUPT              := 0x8000000E
DEFINE LINEERR_INUSE                       := 0x8000000F
DEFINE LINEERR_INVALADDRESS                := 0x80000010
DEFINE LINEERR_INVALADDRESSID              := 0x80000011
DEFINE LINEERR_INVALADDRESSMODE            := 0x80000012
DEFINE LINEERR_INVALADDRESSSTATE           := 0x80000013
DEFINE LINEERR_INVALAPPHANDLE              := 0x80000014
DEFINE LINEERR_INVALAPPNAME                := 0x80000015
DEFINE LINEERR_INVALBEARERMODE             := 0x80000016
DEFINE LINEERR_INVALCALLCOMPLMODE          := 0x80000017
DEFINE LINEERR_INVALCALLHANDLE             := 0x80000018
DEFINE LINEERR_INVALCALLPARAMS             := 0x80000019
DEFINE LINEERR_INVALCALLPRIVILEGE          := 0x8000001A
DEFINE LINEERR_INVALCALLSELECT             := 0x8000001B
DEFINE LINEERR_INVALCALLSTATE              := 0x8000001C
DEFINE LINEERR_INVALCALLSTATELIST          := 0x8000001D
DEFINE LINEERR_INVALCARD                   := 0x8000001E
DEFINE LINEERR_INVALCOMPLETIONID           := 0x8000001F
DEFINE LINEERR_INVALCONFCALLHANDLE         := 0x80000020
DEFINE LINEERR_INVALCONSULTCALLHANDLE      := 0x80000021
DEFINE LINEERR_INVALCOUNTRYCODE            := 0x80000022
DEFINE LINEERR_INVALDEVICECLASS            := 0x80000023
DEFINE LINEERR_INVALDEVICEHANDLE           := 0x80000024
DEFINE LINEERR_INVALDIALPARAMS             := 0x80000025
DEFINE LINEERR_INVALDIGITLIST              := 0x80000026
DEFINE LINEERR_INVALDIGITMODE              := 0x80000027
DEFINE LINEERR_INVALDIGITS                 := 0x80000028
DEFINE LINEERR_INVALEXTVERSION             := 0x80000029
DEFINE LINEERR_INVALGROUPID                := 0x8000002A
DEFINE LINEERR_INVALLINEHANDLE             := 0x8000002B
DEFINE LINEERR_INVALLINESTATE              := 0x8000002C
DEFINE LINEERR_INVALLOCATION               := 0x8000002D
DEFINE LINEERR_INVALMEDIALIST              := 0x8000002E
DEFINE LINEERR_INVALMEDIAMODE              := 0x8000002F
DEFINE LINEERR_INVALMESSAGEID              := 0x80000030
DEFINE LINEERR_INVALPARAM                  := 0x80000032
DEFINE LINEERR_INVALPARKID                 := 0x80000033
DEFINE LINEERR_INVALPARKMODE               := 0x80000034
DEFINE LINEERR_INVALPOINTER                := 0x80000035
DEFINE LINEERR_INVALPRIVSELECT             := 0x80000036
DEFINE LINEERR_INVALRATE                   := 0x80000037
DEFINE LINEERR_INVALREQUESTMODE            := 0x80000038
DEFINE LINEERR_INVALTERMINALID             := 0x80000039
DEFINE LINEERR_INVALTERMINALMODE           := 0x8000003A
DEFINE LINEERR_INVALTIMEOUT                := 0x8000003B
DEFINE LINEERR_INVALTONE                   := 0x8000003C
DEFINE LINEERR_INVALTONELIST               := 0x8000003D
DEFINE LINEERR_INVALTONEMODE               := 0x8000003E
DEFINE LINEERR_INVALTRANSFERMODE           := 0x8000003F
DEFINE LINEERR_LINEMAPPERFAILED            := 0x80000040
DEFINE LINEERR_NOCONFERENCE                := 0x80000041
DEFINE LINEERR_NODEVICE                    := 0x80000042
DEFINE LINEERR_NODRIVER                    := 0x80000043
DEFINE LINEERR_NOMEM                       := 0x80000044
DEFINE LINEERR_NOREQUEST                   := 0x80000045
DEFINE LINEERR_NOTOWNER                    := 0x80000046
DEFINE LINEERR_NOTREGISTERED               := 0x80000047
DEFINE LINEERR_OPERATIONFAILED             := 0x80000048
DEFINE LINEERR_OPERATIONUNAVAIL            := 0x80000049
DEFINE LINEERR_RATEUNAVAIL                 := 0x8000004A
DEFINE LINEERR_RESOURCEUNAVAIL             := 0x8000004B
DEFINE LINEERR_REQUESTOVERRUN              := 0x8000004C
DEFINE LINEERR_STRUCTURETOOSMALL           := 0x8000004D
DEFINE LINEERR_TARGETNOTFOUND              := 0x8000004E
DEFINE LINEERR_TARGETSELF                  := 0x8000004F
DEFINE LINEERR_UNINITIALIZED               := 0x80000050
DEFINE LINEERR_USERUSERINFOTOOBIG          := 0x80000051
DEFINE LINEERR_REINIT                      := 0x80000052
DEFINE LINEERR_ADDRESSBLOCKED              := 0x80000053
DEFINE LINEERR_BILLINGREJECTED             := 0x80000054
DEFINE LINEERR_INVALFEATURE                := 0x80000055
DEFINE LINEERR_NOMULTIPLEINSTANCE          := 0x80000056
DEFINE LINEFEATURE_DEVSPECIFIC     := 0x00000001
DEFINE LINEFEATURE_DEVSPECIFICFEAT := 0x00000002
DEFINE LINEFEATURE_FORWARD         := 0x00000004
DEFINE LINEFEATURE_MAKECALL        := 0x00000008
DEFINE LINEFEATURE_SETMEDIACONTROL := 0x00000010
DEFINE LINEFEATURE_SETTERMINAL     := 0x00000020
DEFINE LINEFORWARDMODE_UNCOND          := 0x00000001
DEFINE LINEFORWARDMODE_UNCONDINTERNAL  := 0x00000002
DEFINE LINEFORWARDMODE_UNCONDEXTERNAL  := 0x00000004
DEFINE LINEFORWARDMODE_UNCONDSPECIFIC  := 0x00000008
DEFINE LINEFORWARDMODE_BUSY            := 0x00000010
DEFINE LINEFORWARDMODE_BUSYINTERNAL    := 0x00000020
DEFINE LINEFORWARDMODE_BUSYEXTERNAL    := 0x00000040
DEFINE LINEFORWARDMODE_BUSYSPECIFIC    := 0x00000080
DEFINE LINEFORWARDMODE_NOANSW          := 0x00000100
DEFINE LINEFORWARDMODE_NOANSWINTERNAL  := 0x00000200
DEFINE LINEFORWARDMODE_NOANSWEXTERNAL  := 0x00000400
DEFINE LINEFORWARDMODE_NOANSWSPECIFIC  := 0x00000800
DEFINE LINEFORWARDMODE_BUSYNA          := 0x00001000
DEFINE LINEFORWARDMODE_BUSYNAINTERNAL  := 0x00002000
DEFINE LINEFORWARDMODE_BUSYNAEXTERNAL  := 0x00004000
DEFINE LINEFORWARDMODE_BUSYNASPECIFIC  := 0x00008000
DEFINE LINEFORWARDMODE_UNKNOWN         := 0x00010000
DEFINE LINEFORWARDMODE_UNAVAIL         := 0x00020000
DEFINE LINEGATHERTERM_BUFFERFULL    := 0x00000001
DEFINE LINEGATHERTERM_TERMDIGIT     := 0x00000002
DEFINE LINEGATHERTERM_FIRSTTIMEOUT  := 0x00000004
DEFINE LINEGATHERTERM_INTERTIMEOUT  := 0x00000008
DEFINE LINEGATHERTERM_CANCEL        := 0x00000010
DEFINE LINEGENERATETERM_DONE    := 0x00000001
DEFINE LINEGENERATETERM_CANCEL  := 0x00000002
DEFINE LINEMEDIACONTROL_NONE           := 0x00000001
DEFINE LINEMEDIACONTROL_START          := 0x00000002
DEFINE LINEMEDIACONTROL_RESET          := 0x00000004
DEFINE LINEMEDIACONTROL_PAUSE          := 0x00000008
DEFINE LINEMEDIACONTROL_RESUME         := 0x00000010
DEFINE LINEMEDIACONTROL_RATEUP         := 0x00000020
DEFINE LINEMEDIACONTROL_RATEDOWN       := 0x00000040
DEFINE LINEMEDIACONTROL_RATENORMAL     := 0x00000080
DEFINE LINEMEDIACONTROL_VOLUMEUP       := 0x00000100
DEFINE LINEMEDIACONTROL_VOLUMEDOWN     := 0x00000200
DEFINE LINEMEDIACONTROL_VOLUMENORMAL   := 0x00000400
DEFINE LINEMEDIAMODE_UNKNOWN           := 0x00000002
DEFINE LINEMEDIAMODE_INTERACTIVEVOICE  := 0x00000004
DEFINE LINEMEDIAMODE_AUTOMATEDVOICE    := 0x00000008
DEFINE LINEMEDIAMODE_DATAMODEM         := 0x00000010
DEFINE LINEMEDIAMODE_G3FAX             := 0x00000020
DEFINE LINEMEDIAMODE_TDD               := 0x00000040
DEFINE LINEMEDIAMODE_G4FAX             := 0x00000080
DEFINE LINEMEDIAMODE_DIGITALDATA       := 0x00000100
DEFINE LINEMEDIAMODE_TELETEX           := 0x00000200
DEFINE LINEMEDIAMODE_VIDEOTEX          := 0x00000400
DEFINE LINEMEDIAMODE_TELEX             := 0x00000800
DEFINE LINEMEDIAMODE_MIXED             := 0x00001000
DEFINE LINEMEDIAMODE_ADSI              := 0x00002000
DEFINE LINEMEDIAMODE_VOICEVIEW         := 0x00004000
DEFINE LAST_LINEMEDIAMODE                   := 0x00004000
DEFINE LINEPARKMODE_DIRECTED       := 0x00000001
DEFINE LINEPARKMODE_NONDIRECTED    := 0x00000002
DEFINE LINEREMOVEFROMCONF_NONE    := 0x00000001
DEFINE LINEREMOVEFROMCONF_LAST    := 0x00000002
DEFINE LINEREMOVEFROMCONF_ANY     := 0x00000003
DEFINE LINEREQUESTMODE_MAKECALL     := 0x00000001
DEFINE LINEREQUESTMODE_MEDIACALL    := 0x00000002
DEFINE LINEREQUESTMODE_DROP         := 0x00000004
DEFINE LAST_LINEREQUESTMODE         := LINEREQUESTMODE_MEDIACALL
DEFINE LINEROAMMODE_UNKNOWN    := 0x00000001
DEFINE LINEROAMMODE_UNAVAIL    := 0x00000002
DEFINE LINEROAMMODE_HOME       := 0x00000004
DEFINE LINEROAMMODE_ROAMA      := 0x00000008
DEFINE LINEROAMMODE_ROAMB      := 0x00000010
DEFINE LINESPECIALINFO_NOCIRCUIT    := 0x00000001
DEFINE LINESPECIALINFO_CUSTIRREG    := 0x00000002
DEFINE LINESPECIALINFO_REORDER      := 0x00000004
DEFINE LINESPECIALINFO_UNKNOWN      := 0x00000008
DEFINE LINESPECIALINFO_UNAVAIL      := 0x00000010
DEFINE LINETERMDEV_PHONE      := 0x00000001
DEFINE LINETERMDEV_HEADSET    := 0x00000002
DEFINE LINETERMDEV_SPEAKER    := 0x00000004
DEFINE LINETERMMODE_BUTTONS       := 0x00000001
DEFINE LINETERMMODE_LAMPS         := 0x00000002
DEFINE LINETERMMODE_DISPLAY       := 0x00000004
DEFINE LINETERMMODE_RINGER        := 0x00000008
DEFINE LINETERMMODE_HOOKSWITCH    := 0x00000010
DEFINE LINETERMMODE_MEDIATOLINE   := 0x00000020
DEFINE LINETERMMODE_MEDIAFROMLINE := 0x00000040
DEFINE LINETERMMODE_MEDIABIDIRECT := 0x00000080
DEFINE LINETERMSHARING_PRIVATE       := 0x00000001
DEFINE LINETERMSHARING_SHAREDEXCL    := 0x00000002
DEFINE LINETERMSHARING_SHAREDCONF    := 0x00000004
DEFINE LINETONEMODE_CUSTOM    := 0x00000001
DEFINE LINETONEMODE_RINGBACK  := 0x00000002
DEFINE LINETONEMODE_BUSY      := 0x00000004
DEFINE LINETONEMODE_BEEP      := 0x00000008
DEFINE LINETONEMODE_BILLING   := 0x00000010
DEFINE LINETRANSFERMODE_TRANSFER    := 0x00000001
DEFINE LINETRANSFERMODE_CONFERENCE  := 0x00000002
DEFINE LINETOLLLISTOPTION_ADD         := 0x00000001
DEFINE LINETOLLLISTOPTION_REMOVE      := 0x00000002
DEFINE LINETRANSLATEOPTION_CARDOVERRIDE           := 0x00000001
DEFINE LINETRANSLATEOPTION_CANCELCALLWAITING      := 0x00000002
DEFINE LINETRANSLATEOPTION_FORCELOCAL             := 0x00000004
DEFINE LINETRANSLATEOPTION_FORCELD                := 0x00000008
DEFINE LINETRANSLATERESULT_CANONICAL              := 0x00000001
DEFINE LINETRANSLATERESULT_INTERNATIONAL          := 0x00000002
DEFINE LINETRANSLATERESULT_LONGDISTANCE           := 0x00000004
DEFINE LINETRANSLATERESULT_LOCAL                  := 0x00000008
DEFINE LINETRANSLATERESULT_INTOLLLIST             := 0x00000010
DEFINE LINETRANSLATERESULT_NOTINTOLLLIST          := 0x00000020
DEFINE LINETRANSLATERESULT_DIALBILLING            := 0x00000040
DEFINE LINETRANSLATERESULT_DIALQUIET              := 0x00000080
DEFINE LINETRANSLATERESULT_DIALDIALTONE           := 0x00000100
DEFINE LINETRANSLATERESULT_DIALPROMPT             := 0x00000200
DEFINE LINELOCATIONOPTION_PULSEDIAL               := 0x00000001
DEFINE LINECARDOPTION_PREDEFINED              := 0x00000001
DEFINE LINECARDOPTION_HIDDEN                  := 0x00000002
#endregion
