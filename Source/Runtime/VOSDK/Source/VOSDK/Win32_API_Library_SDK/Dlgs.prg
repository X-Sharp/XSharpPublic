VOSTRUCT _winCRGB
	MEMBER bRed AS BYTE
	MEMBER bGreen AS BYTE
	MEMBER bBlue AS BYTE
	MEMBER bExtra AS BYTE



#region defines
DEFINE ctlFirst    := 0x0400
DEFINE ctlLast     := 0x04ff
DEFINE psh1        := 0x0400
DEFINE psh2        := 0x0401
DEFINE psh3        := 0x0402
DEFINE psh4        := 0x0403
DEFINE psh5        := 0x0404
DEFINE psh6        := 0x0405
DEFINE psh7        := 0x0406
DEFINE psh8        := 0x0407
DEFINE psh9        := 0x0408
DEFINE psh10       := 0x0409
DEFINE psh11       := 0x040a
DEFINE psh12       := 0x040b
DEFINE psh13       := 0x040c
DEFINE psh14       := 0x040d
DEFINE psh15       := 0x040e
DEFINE pshHelp     := psh15
DEFINE psh16       := 0x040f
//
//  Checkboxes.
//
DEFINE chx1        := 0x0410
DEFINE chx2        := 0x0411
DEFINE chx3        := 0x0412
DEFINE chx4        := 0x0413
DEFINE chx5        := 0x0414
DEFINE chx6        := 0x0415
DEFINE chx7        := 0x0416
DEFINE chx8        := 0x0417
DEFINE chx9        := 0x0418
DEFINE chx10       := 0x0419
DEFINE chx11       := 0x041a
DEFINE chx12       := 0x041b
DEFINE chx13       := 0x041c
DEFINE chx14       := 0x041d
DEFINE chx15       := 0x041e
DEFINE chx16       := 0x041f
//
//  Radio buttons.
//
DEFINE rad1        := 0x0420
DEFINE rad2        := 0x0421
DEFINE rad3        := 0x0422
DEFINE rad4        := 0x0423
DEFINE rad5        := 0x0424
DEFINE rad6        := 0x0425
DEFINE rad7        := 0x0426
DEFINE rad8        := 0x0427
DEFINE rad9        := 0x0428
DEFINE rad10       := 0x0429
DEFINE rad11       := 0x042a
DEFINE rad12       := 0x042b
DEFINE rad13       := 0x042c
DEFINE rad14       := 0x042d
DEFINE rad15       := 0x042e
DEFINE rad16       := 0x042f
//
//  Groups, frames, rectangles, and icons.
//
DEFINE grp1        := 0x0430
DEFINE grp2        := 0x0431
DEFINE grp3        := 0x0432
DEFINE grp4        := 0x0433
DEFINE frm1        := 0x0434
DEFINE frm2        := 0x0435
DEFINE frm3        := 0x0436
DEFINE frm4        := 0x0437
DEFINE rct1        := 0x0438
DEFINE rct2        := 0x0439
DEFINE rct3        := 0x043a
DEFINE rct4        := 0x043b
DEFINE ico1        := 0x043c
DEFINE ico2        := 0x043d
DEFINE ico3        := 0x043e
DEFINE ico4        := 0x043f
//
//  Static text.
//
DEFINE stc1        := 0x0440
DEFINE stc2        := 0x0441
DEFINE stc3        := 0x0442
DEFINE stc4        := 0x0443
DEFINE stc5        := 0x0444
DEFINE stc6        := 0x0445
DEFINE stc7        := 0x0446
DEFINE stc8        := 0x0447
DEFINE stc9        := 0x0448
DEFINE stc10       := 0x0449
DEFINE stc11       := 0x044a
DEFINE stc12       := 0x044b
DEFINE stc13       := 0x044c
DEFINE stc14       := 0x044d
DEFINE stc15       := 0x044e
DEFINE stc16       := 0x044f
DEFINE stc17       := 0x0450
DEFINE stc18       := 0x0451
DEFINE stc19       := 0x0452
DEFINE stc20       := 0x0453
DEFINE stc21       := 0x0454
DEFINE stc22       := 0x0455
DEFINE stc23       := 0x0456
DEFINE stc24       := 0x0457
DEFINE stc25       := 0x0458
DEFINE stc26       := 0x0459
DEFINE stc27       := 0x045a
DEFINE stc28       := 0x045b
DEFINE stc29       := 0x045c
DEFINE stc30       := 0x045d
DEFINE stc31       := 0x045e
DEFINE stc32       := 0x045f
//
//  Listboxes.
//
DEFINE lst1        := 0x0460
DEFINE lst2        := 0x0461
DEFINE lst3        := 0x0462
DEFINE lst4        := 0x0463
DEFINE lst5        := 0x0464
DEFINE lst6        := 0x0465
DEFINE lst7        := 0x0466
DEFINE lst8        := 0x0467
DEFINE lst9        := 0x0468
DEFINE lst10       := 0x0469
DEFINE lst11       := 0x046a
DEFINE lst12       := 0x046b
DEFINE lst13       := 0x046c
DEFINE lst14       := 0x046d
DEFINE lst15       := 0x046e
DEFINE lst16       := 0x046f
//
//  Combo boxes.
//
DEFINE cmb1        := 0x0470
DEFINE cmb2        := 0x0471
DEFINE cmb3        := 0x0472
DEFINE cmb4        := 0x0473
DEFINE cmb5        := 0x0474
DEFINE cmb6        := 0x0475
DEFINE cmb7        := 0x0476
DEFINE cmb8        := 0x0477
DEFINE cmb9        := 0x0478
DEFINE cmb10       := 0x0479
DEFINE cmb11       := 0x047a
DEFINE cmb12       := 0x047b
DEFINE cmb13       := 0x047c
DEFINE cmb14       := 0x047d
DEFINE cmb15       := 0x047e
DEFINE cmb16       := 0x047f
//
//  Edit controls.
//
DEFINE edt1        := 0x0480
DEFINE edt2        := 0x0481
DEFINE edt3        := 0x0482
DEFINE edt4        := 0x0483
DEFINE edt5        := 0x0484
DEFINE edt6        := 0x0485
DEFINE edt7        := 0x0486
DEFINE edt8        := 0x0487
DEFINE edt9        := 0x0488
DEFINE edt10       := 0x0489
DEFINE edt11       := 0x048a
DEFINE edt12       := 0x048b
DEFINE edt13       := 0x048c
DEFINE edt14       := 0x048d
DEFINE edt15       := 0x048e
DEFINE edt16       := 0x048f
//
//  Scroll bars.
//
DEFINE scr1        := 0x0490
DEFINE scr2        := 0x0491
DEFINE scr3        := 0x0492
DEFINE scr4        := 0x0493
DEFINE scr5        := 0x0494
DEFINE scr6        := 0x0495
DEFINE scr7        := 0x0496
DEFINE scr8        := 0x0497
//
//  Controls
//
DEFINE ctl1        := 0x04A0
DEFINE FILEOPENORD      := 1536
DEFINE MULTIFILEOPENORD := 1537
DEFINE PRINTDLGORD      := 1538
DEFINE PRNSETUPDLGORD   := 1539
DEFINE FINDDLGORD       := 1540
DEFINE REPLACEDLGORD    := 1541
DEFINE FONTDLGORD       := 1542
DEFINE FORMATDLGORD31   := 1543
DEFINE FORMATDLGORD30   := 1544
DEFINE RUNDLGORD        := 1545
DEFINE PAGESETUPDLGORD  := 1546
DEFINE NEWFILEOPENORD          := 1547
DEFINE PRINTDLGEXORD           := 1549
DEFINE PAGESETUPDLGORDMOTIF    := 1550
DEFINE COLORMGMTDLGORD         := 1551
DEFINE NEWFILEOPENV2ORD        := 1552
#endregion
