// DATABASE COMBO BOX
#xcommand @ <row>, <col> zGET <var> COMBO                         ;
                        BROWSE < aBrowse_ >                       ;
                        [COLOR <color> ]                          ;
                        [ALIAS <cAlias>]                          ;
                        [RETURN_FIELD < xRetFld>]                 ;
                        [DISPLAY_TAG <cTag>]                      ; 
                        [VALID <valid>]                           ;
                        [WHEN <when>]                             ;
                        [<lDropOnEnter: ALWAYS>]                  ;
                        [XTOP <xTop> ]                            ;
                        [XBOTTOM <xBottom> ]                      ;
                        [FOR <for> ]                              ;
                        [WHILE <while> ]                          ;
                        [BASE_FILTER <cBaseFilter> ]              ;
                        [DISPLAY_FIELD <xDispFld> ]               ;
                        [RETURN_TAG <cKeyTag>  ]                  ;
                        [STRICT <lStrict> ]                       ;
                        [EMPTY_ALLOWED <lEmptyAllowed> ]          ;
                        [HOT_KEYS <aHotKeys_> ]                   ;
                        [PICTURE <cPicture>]                      ;
                        [WIDTH <nWidth>]                          ;
                        [HEIGHT <nHeight> ]                       ;
                        [POST_EVAL <bPostEval>]                   ;
      =>                                                          ;
              zGEDBComboNew( ;
                            <.lDropOnEnter.>,;
                            <cAlias>, <cTag>, ;
                            <xRetFld>, < aBrowse_>,<xDispFld>,;
                            <cKeyTag>, <lStrict>)

// SSUsers
#Command @<Row>,<Col> zGet <cVar> COMBO_USER [STRICT <lStrict>]           ;
                                             [COLOR <Color>]              ;
                                             [FOR   <bFor>]               ;
                                             [VALID <bValid>]             ;
                                             [EMPTY_ALLOWED <lEmpty>]     ;
                                             [WHEN <bWhen>] =>            ;
         @<Row>,<Col> zGet <cVar>                                         ;
         COMBO BROWSE   NIL            ;
         ALIAS          "SSUsers"                                         ;
         RETURN_FIELD   "UserId"                                          ;
         RETURN_TAG     "SSUserSU"                                        ;
         DISPLAY_FIELD  "UserId"                                          ;
         DISPLAY_TAG    "SSUserSU"                                        ;
         XTOP           zGetSysId()                                       ;
         XBOTTOM        zGetSysId()                                       ;
         BASE_FILTER    zGetSysId()                                       ;
         WHILE          nil                ;
         WIDTH          36                                                ;
         HEIGHT         10                                                ;
         [EMPTY_ALLOWED <lEmpty>]                                         ;
         [STRICT        <lStrict>]                                        ;
         [COLOR         <Color>  ]                                        ;
         [FOR           <bFor>   ]                                        ;
         [VALID         <bValid>]                                         ;
         [WHEN          <bWhen>]

