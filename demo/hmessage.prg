/*
 * HWGUI - Harbour GUI library source code:
 * HMessage class
 *
 * Copyright 2021 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#include "hwgui.ch"
#include "hbclass.ch"

#define CLR_WHITE    0xffffff
#define CLR_BLACK    0x000000
#define CLR_DLGHEA   0x2F343F

CLASS HMessage INHERIT HObject

   DATA oFont, oFontHea
   DATA lWndTitle    INIT .T.
   DATA nClrText     INIT CLR_BLACK
   DATA nClrBack     INIT CLR_WHITE
   DATA nClrHeaText  INIT CLR_WHITE
   DATA nClrHeaBack  INIT CLR_DLGHEA
   DATA aStyleBtn
   DATA cOk          INIT "Ok"
   DATA cYes         INIT "Yes"
   DATA cNo          INIT "No"
   DATA cCancel      INIT "Cancel"
   DATA oImgInfo, oImgStop, oImgQue

   METHOD New()
   METHOD Set( oFont, nClrText, nClrBack, lNoWndTitle, oFontHea, nClrHeaText, nClrHeaBack, aStyleBtn )
   METHOD Message( cText, cTitle, oImage, nAlign, ... )
   METHOD MsgInfo( cText, cTitle, nAlign )
   METHOD MsgStop( cText, cTitle, nAlign )
   METHOD MsgYesNo( cText, cTitle, nAlign )
   METHOD MsgYesNoCancel( cText, cTitle, nAlign )
   METHOD MsgGet( cTitle, ... )
ENDCLASS

METHOD New() CLASS HMessage

   RETURN Self

METHOD Set( oFont, nClrText, nClrBack, lNoWndTitle, oFontHea, ;
   nClrHeaText, nClrHeaBack, aStyleBtn ) CLASS HMessage

   IF oFont != Nil; ::oFont := oFont; ENDIF
   IF nClrText != Nil; ::nClrText := nClrText; ENDIF
   IF nClrBack != Nil; ::nClrBack := nClrBack; ENDIF
   IF !Empty( lNoWndTitle ); ::lWndTitle := .F.; ENDIF
   IF oFontHea != Nil; ::oFontHea := oFontHea; ENDIF
   IF nClrHeaText != Nil; ::nClrHeaText := nClrHeaText; ENDIF
   IF nClrHeaBack != Nil; ::nClrHeaBack := nClrHeaBack; ENDIF
   IF aStyleBtn != Nil; ::aStyleBtn := aStyleBtn; ENDIF

   RETURN Nil

METHOD Message( cText, cTitle, oImage, nAlign, ... ) CLASS HMessage

   LOCAL oDlg, oPanelH, arr, i, nLenMax := 0, nLineHeight, nBtnLenMax := 60, nDlgWidth, x1, y1 := 20
   LOCAL aParams := hb_aParams()
   LOCAL hDC, hFont
   LOCAL bPaintHea := {|o,hDC|
      HB_SYMBOL_UNUSED( o )
      hwg_Drawtransparentbitmap( hDC, oImage:handle, 8, Int( (oPanelH:nHeight-oImage:nHeight)/2 ), CLR_WHITE )
      RETURN .T.
   }
   MEMVAR nRes
   PRIVATE nRes := 0

   IF Empty( cTitle); cTitle := ""; ENDIF
   IF Empty( cText); cText := ""; ENDIF

   arr := hb_aTokens( cText, ';' )

   hDC := hwg_Getdc( HWindow():GetMain():handle )
   IF ::oFont != Nil
      hFont := hwg_Selectobject( hDC, ::oFont:handle )
   ENDIF
   FOR i := 1 TO Len( arr )
      nLenMax := Max( nLenMax, hwg_GetTextSize( hDC, arr[i] )[1] )
   NEXT
   nLineHeight := hwg_GetTextSize( hDC, arr[1] )[2] + 4
   FOR i := 5 TO Len( aParams )
      nBtnLenMax := Max( nBtnLenMax, hwg_GetTextSize( hDC, aParams[i] )[1] )
   NEXT
#ifndef __PLATFORM__UNIX
   IF ::oFont != Nil
      hwg_Selectobject( hDC, hFont )
   ENDIF
#endif
   hwg_ReleaseDC( HWindow():GetMain():handle, hDC )
   nLenMax += 32
   nBtnLenMax += 32
   nDlgWidth := Max( nLenMax + 40, (nBtnLenMax+16) * (Len(aParams)-4) + 24 )

   IF ::lWndTitle
      INIT DIALOG oDlg TITLE cTitle BACKCOLOR ::nClrBack ;
         SIZE nDlgWidth, (Len(arr) * nLineHeight+4) + 120 FONT ::oFont
   ELSE
      INIT DIALOG oDlg TITLE cTitle BACKCOLOR ::nClrBack ;
         SIZE nDlgWidth, (Len(arr) * nLineHeight+4) + 140 STYLE WND_NOTITLE + WND_NOSIZEBOX FONT ::oFont
      ADD HEADER PANEL oPanelH HEIGHT 32 TEXTCOLOR ::nClrHeaText BACKCOLOR ::nClrHeaBack ;
         FONT ::oFontHea TEXT cTitle COORS 20
      IF !Empty( oImage )
        oPanelH:xt := oImage:nWidth + 24
        //oPanelH:SetPaintCB( PAINT_ITEM, bPaintHea )
        oPanelH:oPaintCB := HPaintCB():New()
        oPanelH:oPaintCB:Set( PAINT_ITEM, bPaintHea )
      ENDIF
      y1 += oPanelH:nHeight
   ENDIF

   IF nAlign == Nil
      nAlign := SS_CENTER
   ENDIF
   x1 := Int((nDlgWidth - nLenMax) / 2 )
   FOR i := 1 TO Len( arr )
      @ x1, y1+(i-1)*(nLineHeight+4) SAY arr[i] SIZE nLenMax, nLineHeight ;
         STYLE nAlign COLOR ::nClrText TRANSPARENT
   NEXT

   x1 := Int( ( nDlgWidth - (nBtnLenMax+16) * (Len(aParams)-4) + 16 ) / 2 )
   FOR i := 5 TO Len( aParams )
      IF Empty( ::aStyleBtn )
         @ x1 + (i-5)*(nBtnLenMax+16), oDlg:nHeight-48 BUTTON aParams[i] SIZE nBtnLenMax, 32 ;
            ON CLICK &( "{||nRes:="+Ltrim(Str(i-4))+",hwg_EndDialog()}" )
      ELSE
         @ x1 + (i-5)*(nBtnLenMax+16), oDlg:nHeight-48 OWNERBUTTON SIZE nBtnLenMax, 32 ;
            TEXT aParams[i] COLOR CLR_BLACK ON CLICK &( "{||nRes:="+Ltrim(Str(i-4))+",hwg_EndDialog()}" )
         ATail(oDlg:aControls):aStyle := ::aStyleBtn
      ENDIF
   NEXT

   ACTIVATE DIALOG oDlg CENTER

   RETURN nRes

METHOD MsgInfo( cText, cTitle, nAlign ) CLASS HMessage

   RETURN ::Message( cText, cTitle, ::oImgInfo, nAlign, ::cOk )

METHOD MsgStop( cText, cTitle, nAlign ) CLASS HMessage

   RETURN ::Message( cText, cTitle, ::oImgStop, nAlign, ::cOk )

METHOD MsgYesNo( cText, cTitle, nAlign ) CLASS HMessage

   LOCAL n := ::Message( cText, cTitle, ::oImgQue, nAlign, ::cYes, ::cNo )

   RETURN (n == 1)

METHOD MsgYesNoCancel( cText, cTitle, nAlign ) CLASS HMessage

   LOCAL n := ::Message( cText, cTitle, ::oImgQue, nAlign, ::cYes, ::cNo, ::cCancel )

   RETURN Iif( n==3, 0, n )

METHOD MsgGet( cTitle, ... ) CLASS HMessage

   LOCAL oDlg, oPanelH, i, j, nSayMax := 0, nGetMax := 0, nLineHeight, nBtnLenMax := 90
   LOCAL xTemp, nDlgWidth, x1, y1 := 20, l
   LOCAL aParams := hb_aParams()
   LOCAL hDC, hFont
   MEMVAR get1, get2, get3, get4, get5
   PRIVATE get1, get2, get3, get4, get5

   hDC := hwg_Getdc( HWindow():GetMain():handle )
   IF ::oFont != Nil
      hFont := hwg_Selectobject( hDC, ::oFont:handle )
   ENDIF

   FOR i := 2 TO Len( aParams )
      nSayMax := Max( nSayMax, hwg_GetTextSize( hDC, aParams[i,1] )[1] )
      IF !Empty( aParams[i,2] )
         nGetMax := Max( nGetMax, hwg_GetTextSize( hDC, aParams[i,2] )[1] )
      ENDIF
      IF ( xTemp := Valtype( aParams[i,3] ) ) == "A"
         l := ( Valtype( aParams[i,3,1] ) == "A" )
         FOR j := 1 TO Len( aParams[i,3] )
            nGetMax := Max( nGetMax, hwg_GetTextSize( hDC, Iif( l, aParams[i,3,j,1], aParams[i,3,j] ) )[1] )
         NEXT
         nGetMax += 20
      ELSEIF xTemp == "C"
         nGetMax := Max( nGetMax, hwg_GetTextSize( hDC, aParams[i,3] )[1] )
      ENDIF
   NEXT
   nLineHeight := hwg_GetTextSize( hDC, aParams[2,1] )[2] + 2
#ifndef __PLATFORM__UNIX
   IF ::oFont != Nil
      hwg_Selectobject( hDC, hFont )
   ENDIF
#endif
   hwg_ReleaseDC( HWindow():GetMain():handle, hDC )
   nSayMax += 32
   nGetMax += 24
   nDlgWidth := Max( nSayMax + nGetMax + 40, (nBtnLenMax+16) * 2 + 24 )

   IF ::lWndTitle
      INIT DIALOG oDlg TITLE cTitle BACKCOLOR ::nClrBack ;
         SIZE nDlgWidth, ((Len(aParams)-1) * nLineHeight+4) + 100
   ELSE
      INIT DIALOG oDlg TITLE cTitle BACKCOLOR ::nClrBack ;
         SIZE nDlgWidth, ((Len(aParams)-1) * nLineHeight+4) + 120 STYLE WND_NOTITLE + WND_NOSIZEBOX
      ADD HEADER PANEL oPanelH HEIGHT 32 TEXTCOLOR ::nClrHeaText BACKCOLOR ::nClrHeaBack ;
         FONT ::oFontHea TEXT cTitle COORS 20
      y1 += oPanelH:nHeight
   ENDIF

   x1 := 16
   FOR i := 2 TO Len( aParams )
      @ x1, y1+(i-2)*(nLineHeight+4) SAY aParams[i,1] SIZE nSayMax, nLineHeight ;
         COLOR ::nClrText TRANSPARENT
      IF Len( aParams[i] ) == 3
         AAdd( aParams[i], Nil )
      ENDIF
      aParams[i,4] := Iif( Valtype( aParams[i,3] ) == "A", 0, aParams[i,3] )
   NEXT

   x1 += nSayMax + 10

   i := 2
   get1 := aParams[i,4]
   IF Valtype( aParams[i,3] ) == "A"
      @ x1, y1 GET COMBOBOX get1 ITEMS aParams[i,3] SIZE nGetMax, nLineHeight STYLE WS_TABSTOP
   ELSE
      @ x1, y1 GET get1 SIZE nGetMax, nLineHeight STYLE WS_TABSTOP MAXLENGTH 0
   ENDIF

   i ++
   IF i <= Len( aParams )
      y1 += nLineHeight + 4
      get3 := aParams[i,4]
      IF Valtype( aParams[i,3] ) == "A"
         @ x1, y1 GET COMBOBOX get2 ITEMS aParams[i,3] SIZE nGetMax, nLineHeight STYLE WS_TABSTOP
      ELSE
         @ x1, y1 GET get2 SIZE nGetMax, nLineHeight STYLE WS_TABSTOP MAXLENGTH 0
      ENDIF
   ENDIF

   i ++
   IF i <= Len( aParams )
      y1 += nLineHeight + 4
      get3 := aParams[i,4]
      IF Valtype( aParams[i,3] ) == "A"
         @ x1, y1 GET COMBOBOX get3 ITEMS aParams[i,3] SIZE nGetMax, nLineHeight STYLE WS_TABSTOP
      ELSE
         @ x1, y1 GET get3 SIZE nGetMax, nLineHeight STYLE WS_TABSTOP MAXLENGTH 0
      ENDIF
   ENDIF

   i ++
   IF i <= Len( aParams )
      y1 += nLineHeight + 4
      get3 := aParams[i,4]
      IF Valtype( aParams[i,3] ) == "A"
         @ x1, y1 GET COMBOBOX get4 ITEMS aParams[i,3] SIZE nGetMax, nLineHeight STYLE WS_TABSTOP
      ELSE
         @ x1, y1 GET get4 SIZE nGetMax, nLineHeight STYLE WS_TABSTOP MAXLENGTH 0
      ENDIF
   ENDIF

   i ++
   IF i <= Len( aParams )
      y1 += nLineHeight + 4
      get3 := aParams[i,4]
      IF Valtype( aParams[i,3] ) == "A"
         @ x1, y1 GET COMBOBOX get5 ITEMS aParams[i,3] SIZE nGetMax, nLineHeight STYLE WS_TABSTOP
      ELSE
         @ x1, y1 GET get5 SIZE nGetMax, nLineHeight STYLE WS_TABSTOP MAXLENGTH 0
      ENDIF
   ENDIF

   x1 := Int( ( nDlgWidth - (nBtnLenMax+16) * 2 + 16 ) / 2 )
   IF Empty( ::aStyleBtn )
      @ x1, oDlg:nHeight-48 BUTTON ::cOk SIZE nBtnLenMax, 32 ;
         ON CLICK {||oDlg:lResult := .T., hwg_EndDialog()}
      @ x1 + nBtnLenMax+16, oDlg:nHeight-48 BUTTON ::cCancel SIZE nBtnLenMax, 32 ;
         ON CLICK {||oDlg:lResult := .T., hwg_EndDialog()}
   ELSE
      @ x1, oDlg:nHeight-48 OWNERBUTTON SIZE nBtnLenMax, 32 ;
         TEXT ::cOk COLOR CLR_BLACK ON CLICK {||oDlg:lResult := .T., hwg_EndDialog()}
      ATail(oDlg:aControls):aStyle := ::aStyleBtn
      @ x1 + nBtnLenMax+16, oDlg:nHeight-48 OWNERBUTTON SIZE nBtnLenMax, 32 ;
         TEXT ::cCancel COLOR CLR_BLACK ON CLICK {||oDlg:lResult := .T., hwg_EndDialog()}
      ATail(oDlg:aControls):aStyle := ::aStyleBtn
   ENDIF

   ACTIVATE DIALOG oDlg CENTER

   IF oDlg:lResult

      FOR i := 2 TO Len( aParams )
         aParams[i,4] := __mvGet( "get" + Ltrim(Str(i-1)) )
      NEXT
   ENDIF

   RETURN oDlg:lResult