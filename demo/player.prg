/*
 * Audio player demo program, main source file
 * Harbour + HwGUI + Miniaudio
 *
 * HbAudio - Harbour wrappers for miniaudio
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "hwgui.ch"

#define CLR_BLACK   0
#define CLR_WHITE   0xffffff
#define CLR_BGRAY1  0x7b7680   // Sys buttons
#define CLR_BGRAY2  0x5b5760
#define CLR_DBROWN  0x2F343F   // Header pane
#define CLR_GBROWN  0x3C3940

#define HEA_HEIGHT         28
#define PL_WIDTH          600
#define PL_HEIGHT          32

STATIC oPlayer
STATIC arrColors := { {CLR_BGRAY1,CLR_BGRAY2,CLR_DBROWN,CLR_GBROWN,CLR_BLACK,CLR_WHITE}, ;
   {0x797979,0x555555,0x222222,0x353535,CLR_BLACK,CLR_WHITE} }
STATIC nTheme := 1, cLastPath, oFontMain, nVolume := 0.5, lTime := .T., lGraph := .F.

FUNCTION Main( cFile )

   LOCAL oMain, oPaneHea, oPaneTop

   ReadIni()

   IF Empty( oFontMain )
      PREPARE FONT oFontMain NAME "Georgia" WIDTH 0 HEIGHT - 15 ITALIC
   ENDIF

   INIT WINDOW oMain MAIN TITLE "" AT 200, 0 SIZE PL_WIDTH, HEA_HEIGHT+PL_HEIGHT ;
      FONT oFontMain STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {||oPlayer:KillSound()}

   ADD HEADER PANEL oPaneHea HEIGHT HEA_HEIGHT TEXTCOLOR arrColors[nTheme,6] BACKCOLOR arrColors[nTheme,3] ;
      FONT oFontMain TEXT "HbPlayer" COORS 20 BTN_CLOSE BTN_MINIMIZE

   oPaneHea:SetSysbtnColor( arrColors[nTheme,6], arrColors[nTheme,1] )

   @ 0, HEA_HEIGHT PANEL oPaneTop SIZE oMain:nWidth, PL_HEIGHT ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   oPlayer := HPlayer():New( oPaneTop, oPaneHea, arrColors[nTheme], cLastPath, nVolume, lTime, lGraph )

   SET KEY 0, VK_SPACE TO Iif( oPlayer:lStopped, oPlayer:Play(), oPlayer:Stop() )

   ACTIVATE WINDOW oMain ON ACTIVATE {|| Iif(!Empty(cFile), oPlayer:PlayFile(cFile), .T.) }

   IF !Empty( HPlayer():pEngine )
      ma_Engine_UnInit( HPlayer():pEngine )
      HPlayer():pEngine := Nil
   ENDIF

   RETURN Nil

STATIC FUNCTION ReadIni()

   LOCAL hIni := _IniRead( hb_Dirbase() + "player.ini" ), aIni, nSect, aSect, cTemp

   IF !Empty( hIni )
      aIni := hb_hKeys( hIni )
      FOR nSect := 1 TO Len( aIni )
         IF Upper(aIni[nSect]) == "MAIN"
            IF !Empty( aSect := hIni[ aIni[nSect] ] )
               hb_hCaseMatch( aSect, .F. )
               IF hb_hHaskey( aSect, cTemp := "path" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cLastPath := cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "fontmain" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  oFontMain := HFont():LoadFromStr( cTemp )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "volume" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nVolume := Val( cTemp )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "theme" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nTheme := Val( cTemp )
                  IF nTheme <= 0 .OR. nTheme > Len( arrColors )
                     nTheme := 1
                  ENDIF
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "showtime" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lTime := ( Lower(cTemp) == "on" )
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "showgraph" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  lGraph := ( Lower(cTemp) == "on" )
               ENDIF
            ENDIF
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