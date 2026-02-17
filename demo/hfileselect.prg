/*
 * HFileSelect class - file selection dialog implementation
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

#define HEA_HEIGHT 28
#define BOTTOM_HEIGHT 60

CLASS HFileSelect

   DATA oDlg
   DATA aColors
   DATA cCurrPath
   DATA oStyleNormal, oStylePressed, oStyleOver

   METHOD New( aColors, cCurrPath )
   METHOD Show()

ENDCLASS

METHOD New( aColors, cCurrPath ) CLASS HFileSelect

   ::aColors := Iif( Empty(aColors), ;
      {CLR_BGRAY1,CLR_BGRAY2,CLR_DBROWN,CLR_GBROWN,CLR_BLACK,CLR_WHITE}, aColors )
   ::cCurrPath := Iif( Empty(cCurrPath), hb_DirBase(), cCurrPath )

   ::oStyleNormal := HStyle():New( {::aColors[CLR_BOARD]}, 1 )
   ::oStylePressed := HStyle():New( {::aColors[CLR_STYLE]}, 1,, 2, ::aColors[CLR_BTN1] )
   ::oStyleOver := HStyle():New( {::aColors[CLR_STYLE]}, 1 )

   RETURN Self

METHOD Show() CLASS HFileSelect

   LOCAL oDlg, oPaneHea, oPaneTop, oBrw1, oBrw2
   LOCAL oFont := HWindow():Getmain():oFont, lRes := .F.

   INIT DIALOG oDlg TITLE "" BACKCOLOR ::aColors[CLR_DLG] FONT oFont ;
      AT 100, 100 SIZE 700, 600 STYLE WND_NOTITLE

   ::oDlg := oDlg

   ADD HEADER PANEL oPaneHea HEIGHT HEA_HEIGHT TEXTCOLOR ::aColors[CLR_BTN2] ;
      BACKCOLOR ::aColors[CLR_HEAD] FONT oFont TEXT "File selection" COORS 20 BTN_CLOSE
   oPaneHea:SetSysbtnColor( ::aColors[CLR_BTN2], ::aColors[CLR_BOARD] )

   @ 0, oPaneHea:nHeight PANEL oPaneTop SIZE oDlg:nWidth, HEA_HEIGHT+4 ;
      BACKCOLOR ::aColors[CLR_STYLE] ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   @ 0, oPaneHea:nHeight+oPaneTop:nHeight BROWSE oBrw1 ARRAY ;
      SIZE 180, oDlg:nHeight-oPaneHea:nHeight-oPaneTop:nHeight-BOTTOM_HEIGHT ;
      ON SIZE ANCHOR_TOPABS + ANCHOR_BOTTOMABS NO VSCROLL
   oBrw1:aArray := SetBrw1( Self )

   @ oBrw1:nWidth+4, oBrw1:nTop BROWSE oBrw2 ARRAY ;
      SIZE oDlg:nWidth-oBrw1:nWidth-4, oBrw1:nHeight ;
      ON SIZE ANCHOR_TOPABS + ANCHOR_BOTTOMABS + ANCHOR_RIGHTABS NO VSCROLL
   oBrw2:aArray := SetBrw2( Self )

   @ @ oBrw1:nWidth, oBrw1:nTop SPLITTER SIZE 4,oBrw1:nHeight DIVIDE {oBrw1} FROM {oBrw2}

   @ oDlg:nWidth-240, oDlg:nHeight-48 OWNERBUTTON SIZE 100,30 TEXT "Cancel" ;
      COLOR ::aColors[CLR_BTN1] HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {||hwg_EndDialog()}

   @ oDlg:nWidth-120, oDlg:nHeight-48 OWNERBUTTON SIZE 100,30 TEXT "Select" ;
      COLOR ::aColors[CLR_BTN1] HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {||lRes := .T., hwg_EndDialog()}

   ACTIVATE DIALOG oDlg

   IF lRes
   ENDIF

   RETURN ""

STATIC FUNCTION SetBrw1( o )

   RETURN { "Home" }

STATIC FUNCTION SetBrw2( o )

   LOCAL arr

   arr := Directory( o:cCurrPath+"*", "HSD" )

   RETURN arr