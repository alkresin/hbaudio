/*
 * Player
 */

#include "hbclass.ch"
#include "hwgui.ch"

#define CLR_BLACK   0
#define CLR_WHITE   0xffffff
#define CLR_BGRAY1  0x7b7680
#define CLR_BGRAY2  0x5b5760
#define CLR_DBROWN  0x2F343F
#define CLR_GBROWN  0x3C3940

#define HEA_HEIGHT         28
#define PL_WIDTH          600
#define PL_HEIGHT          32

STATIC oPlayer

FUNCTION Main( cFile )

   LOCAL oMain, oPaneHea, oPaneTop, oFont

   ReadIni()

   PREPARE FONT oFont NAME "Georgia" WIDTH 0 HEIGHT - 15 ITALIC

   INIT WINDOW oMain MAIN TITLE "" AT 200, 0 SIZE PL_WIDTH, HEA_HEIGHT+PL_HEIGHT  ;
      BACKCOLOR CLR_GBROWN FONT oFont STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {||oPlayer:KillSound()}

   ADD HEADER PANEL oPaneHea HEIGHT HEA_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR CLR_DBROWN ;
      FONT oFont TEXT "HbPlayer" COORS 20 BTN_CLOSE BTN_MINIMIZE

   oPaneHea:SetSysbtnColor( CLR_WHITE, CLR_BGRAY1 )

   @ 0, HEA_HEIGHT PANEL oPaneTop SIZE oMain:nWidth, PL_HEIGHT ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   oPlayer := HPlayer():New( oPaneTop, oPaneHea )

   SET KEY 0, VK_SPACE TO Iif( oPlayer:lStopped, oPlayer:Play(), oPlayer:Stop() )

   ACTIVATE WINDOW oMain ON ACTIVATE {|| Iif(!Empty(cFile), oPlayer:PlayFile(cFile), .T.) }

   IF !Empty( HPlayer():pEngine )
      ma_Engine_UnInit( HPlayer():pEngine )
      HPlayer():pEngine := Nil
   ENDIF

   RETURN Nil

STATIC FUNCTION ReadIni()

   LOCAL hIni := _IniRead( hb_Dirbase() + "player.ini" ), aIni, nSect

   IF !Empty( hIni )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "OPTIONS"
         ENDIF
      NEXT
   ENDIF

   RETURN Nil

EXIT PROCEDURE PExit

   oPlayer:End()
   IF !Empty( HPlayer():pEngine )
      ma_Engine_UnInit( HPlayer():pEngine )
      HPlayer():pEngine := Nil
   ENDIF

   RETURN