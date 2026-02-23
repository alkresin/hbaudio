/*
 * Voice recorder demo program, HRecorder class source file
 * Harbour + HwGUI + Miniaudio
 *
 * HbAudio - Harbour wrappers for miniaudio
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hbclass.ch"
#include "hwgui.ch"

#define CLR_BLACK   0
#define CLR_WHITE   0xffffff
#define CLR_BGRAY1  0x7b7680   // Sys buttons
#define CLR_BGRAY2  0x5b5760
#define CLR_DBROWN  0x2F343F   // Header pane
#define CLR_GBROWN  0x3C3940

#define CLR_BOARD   1          // oStyleNormal, Board, buttons, tracker
#define CLR_STYLE   2          // oStylePressed, oStyleOver
#define CLR_HEAD    3          // Dialog header pane
#define CLR_DLG     4          // Dialog back color
#define CLR_BTN1    5
#define CLR_BTN2    6

#define HEA_HEIGHT         28

CLASS HRecorder

   DATA oBoard, oBtnAdd, oBtnRec, oSayState
   DATA cFile        INIT "out.wav"
   DATA aColors
   DATA oBrushBtn1, oBrushBtn2
   DATA oStyleNormal, oStylePressed, oStyleOver
   DATA cLastPath
   DATA lRecording   INIT .F.
   DATA pDevice

   METHOD New( oPane, oWnd, aColors )
   METHOD RecFile()
   METHOD Record()
   METHOD Stop()
   METHOD KillDevice()
   METHOD Message( cText, cTitle )
   METHOD End()

   ENDCLASS

METHOD New( oPane, aColors ) CLASS HRecorder

   LOCAL bPaintB1 := {|o,hdc|
      hwg_Ellipse_Filled( hDC, o:nLeft+4, o:nTop+4, o:nLeft+o:nWidth-4, o:nTop+o:nHeight-4,, ;
         Iif( ::lRecording, ::oBrushBtn1:handle, ::oBrushBtn2:handle ) )
      RETURN 0
   }
   LOCAL bPaintB2 := {|o,hdc|
      LOCAL nl := o:nLeft, nt := o:nTop, oStyle
      oStyle := Iif( Len(o:aStyles) > o:nState, o:aStyles[o:nState + 1], ATail(o:aStyles) )
      oStyle:Draw( hDC, o:nLeft, o:nTop, o:nLeft+o:nWidth-1, o:nTop+o:nHeight-1 )
      hwg_Rectangle_Filled( hDC, nl+4+Int((o:nWidth-8)/2)-1, nt+6, nl+4+Int((o:nWidth-8)/2)+1, nt+o:nHeight-6, .F., ::oBrushBtn2:handle )
      hwg_Rectangle_Filled( hDC, nl+6, nt+4+Int((o:nHeight-8)/2)-1, nl+o:nWidth-4, nt+6+Int((o:nHeight-12)/2)+1, .F., ::oBrushBtn2:handle )
      RETURN 0
   }

   ::aColors := Iif( Empty(aColors), ;
      {CLR_BGRAY1,CLR_BGRAY2,CLR_DBROWN,CLR_GBROWN,CLR_BLACK,CLR_WHITE}, aColors )
   ::cLastPath := hb_DirBase()

   ::oBrushBtn1 := HBrush():Add( ::aColors[CLR_BTN1] )
   ::oBrushBtn2 := HBrush():Add( ::aColors[CLR_BTN2] )
   ::oStyleNormal := HStyle():New( {::aColors[CLR_BOARD]}, 1 )
   ::oStylePressed := HStyle():New( {::aColors[CLR_STYLE]}, 1,, 2, ::aColors[CLR_BTN1] )
   ::oStyleOver := HStyle():New( {::aColors[CLR_STYLE]}, 1 )

   @ 0, 0 BOARD ::oBoard SIZE oPane:nWidth, oPane:nHeight OF oPane BACKCOLOR ::aColors[CLR_BOARD] ;
      ON SIZE ANCHOR_LEFTABS+ANCHOR_RIGHTABS

   @ 2, 2 DRAWN ::oBtnAdd SIZE 20, 28 ;
      HSTYLES { ::oStyleNormal, ::oStyleOver, ::oStyleNormal }
   ::oBtnAdd:bPaint := bPaintB2
   ::oBtnAdd:bClick := {|| ::RecFile() }

   @ 24, 2 DRAWN ::oBtnRec SIZE 20, 28 ;
      HSTYLES { ::oStyleNormal, ::oStyleOver, ::oStyleNormal }
   ::oBtnRec:bPaint := bPaintB1
   ::oBtnRec:bClick := {|| ::Record() }
   ::oBtnRec:lHide := .T.

   @ 50, 2 DRAWN ::oSayState SIZE 160, 28 COLOR ::aColors[CLR_BTN2] BACKCOLOR ::aColors[CLR_BOARD]

   RETURN Self

METHOD RecFile() CLASS HRecorder

   LOCAL oDlg, oPaneHea, oEdit, oFont := HWindow():Getmain():oFont
   LOCAL cFile := "out.wav"
   LOCAL bFile := {||
      cFile := HFileSelect():Save( { {"Wav files","*.wav"} }, ::cLastPath, ::aColors )
      IF !Empty( cFile )
         oEdit:SetText( hb_fnameNameExt( cFile ) )
         ::cLastPath := hb_fnameDir( cFile )
         ::oSayState:SetText( hb_fnameNameExt( cFile ) )
      ENDIF
      RETURN .T.
   }
   LOCAL bOk := {||
      IF Empty( ::pDevice := ma_capture_init( cFile, 44100, 2 ) )
         ::Message( "Capture init failed", "Error" )
         hwg_EndDialog()
         RETURN .F.
      ENDIF
      ::oBtnRec:lHide := .F.
      ::oBtnRec:Refresh()
      hwg_EndDialog()
      RETURN .T.
   }

   IF ::lRecording
      RETURN .F.
   ENDIF

   INIT DIALOG oDlg TITLE "" BACKCOLOR ::aColors[CLR_DLG] FONT oFont ;
      AT 300, 58 SIZE 280, 200 STYLE WND_NOTITLE

   ADD HEADER PANEL oPaneHea HEIGHT HEA_HEIGHT TEXTCOLOR ::aColors[CLR_BTN2] BACKCOLOR ::aColors[CLR_HEAD] ;
      FONT oFont TEXT "Add record" COORS 20 BTN_CLOSE
   oPaneHea:SetSysbtnColor( ::aColors[CLR_BTN2], ::aColors[CLR_BOARD] )

   @ 20, 40 EDITBOX oEdit CAPTION ::cFile SIZE 200,26
   @ 220,40 OWNERBUTTON SIZE 40,26 TEXT "..." COLOR ::aColors[CLR_BTN1] ;
         HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
         ON CLICK bFile

   @ 80,oDlg:nHeight-40 OWNERBUTTON SIZE 100,30 TEXT "Ok" COLOR ::aColors[CLR_BTN1] ;
         HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
         ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
         ON CLICK bOk

   ACTIVATE DIALOG oDlg

   RETURN .T.

METHOD Record() CLASS HRecorder

   IF Empty( ::pDevice ) .OR. ::lRecording
      RETURN .F.
   ENDIF

   ::oBtnAdd:lHide := .T.
   ::oBtnAdd:Refresh()

   ::oBtnRec:bClick := {||::Stop()}
   ::oBtnRec:Refresh()
   ::oSayState:SetText( "Recording" )

   ma_capture_start( ::pDevice )
   ::lRecording := .T.

   RETURN .T.

METHOD Stop() CLASS HRecorder

   IF Empty( ::pDevice ) .OR. !::lRecording
      RETURN .F.
   ENDIF

   ::oBtnAdd:lHide := .F.
   ::oBtnAdd:Refresh()

   ::oBtnRec:bClick := {||::Record()}
   ::oBtnRec:Refresh()
   ::oSayState:SetText( hb_fnameNameExt( ::cFile ) )

   ::KillDevice()

   RETURN .T.

METHOD KillDevice() CLASS HRecorder

   IF !Empty( ::pDevice )
      IF ::lRecording
         ma_capture_stop( ::pDevice )
      ENDIF
      ma_capture_uninit( ::pDevice )
      ::pDevice := Nil
   ENDIF
   ::lRecording := .F.

   RETURN .T.

METHOD Message( cText, cTitle ) CLASS HRecorder

   LOCAL oDlg, oPanelH, arr, i, nLenMax := 0, nLineHeight, nBtnLenMax := 60, nDlgWidth, x1, y1 := 20
   LOCAL oFont := HWindow():Getmain():oFont, nAlign
   LOCAL hDC, hFont

   IF Empty( cTitle); cTitle := ""; ENDIF
   IF Empty( cText); cText := ""; ENDIF

   arr := hb_aTokens( cText, ';' )

   hDC := hwg_Getdc( HWindow():GetMain():handle )
   hFont := hwg_Selectobject( hDC, oFont:handle )
   FOR i := 1 TO Len( arr )
      nLenMax := Max( nLenMax, hwg_GetTextSize( hDC, arr[i] )[1] )
   NEXT
   nLineHeight := hwg_GetTextSize( hDC, arr[1] )[2] + 4
#ifndef __PLATFORM__UNIX
   hwg_Selectobject( hDC, hFont )
#endif
   hwg_ReleaseDC( HWindow():GetMain():handle, hDC )
   nLenMax += 32
   nBtnLenMax += 32
   nDlgWidth := Max( nLenMax + 40, (nBtnLenMax+16) + 24 )

   INIT DIALOG oDlg TITLE cTitle BACKCOLOR ::aColors[CLR_DLG] ;
      SIZE nDlgWidth, (Len(arr) * nLineHeight+4) + 140 STYLE WND_NOTITLE + WND_NOSIZEBOX FONT oFont

   ADD HEADER PANEL oPanelH HEIGHT HEA_HEIGHT TEXTCOLOR ::aColors[CLR_BTN2] BACKCOLOR ::aColors[CLR_HEAD] ;
      FONT oFont TEXT cTitle COORS 20
   y1 += oPanelH:nHeight

   nAlign := SS_CENTER

   x1 := Int((nDlgWidth - nLenMax) / 2 )
   FOR i := 1 TO Len( arr )
      @ x1, y1+(i-1)*(nLineHeight+4) SAY arr[i] SIZE nLenMax, nLineHeight ;
         STYLE nAlign COLOR ::aColors[CLR_BTN2] TRANSPARENT
   NEXT

   @ Int( ( nDlgWidth - nBtnLenMax ) / 2 ), oDlg:nHeight-48 OWNERBUTTON SIZE nBtnLenMax, 32 ;
      TEXT "Close" COLOR ::aColors[CLR_BTN1] ;
      HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {||hwg_EndDialog()}

   ACTIVATE DIALOG oDlg CENTER

   RETURN .T.

METHOD End() CLASS HRecorder

   ::KillDevice()

   RETURN .T.