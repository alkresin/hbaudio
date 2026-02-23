/*
 * HbRecorder - voice recorder demo program, main source file
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
#define PL_WIDTH          400
#define PL_HEIGHT          32

STATIC oRecord
STATIC arrColors := { {CLR_BGRAY1,CLR_BGRAY2,CLR_DBROWN,CLR_GBROWN,CLR_BLACK,CLR_WHITE}, ;
   {0x797979,0x555555,0x222222,0x353535,CLR_BLACK,CLR_WHITE} }
STATIC nTheme := 1, cLastPath, oFontMain

FUNCTION Main()

   LOCAL oMain, oPaneHea, oPaneTop

   IF Empty( oFontMain )
      PREPARE FONT oFontMain NAME "Georgia" WIDTH 0 HEIGHT - 15 ITALIC
   ENDIF

   INIT WINDOW oMain MAIN TITLE "" AT 200, 0 SIZE PL_WIDTH, HEA_HEIGHT+PL_HEIGHT ;
      FONT oFontMain STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {||.T.}

   ADD HEADER PANEL oPaneHea HEIGHT HEA_HEIGHT TEXTCOLOR arrColors[nTheme,6] BACKCOLOR arrColors[nTheme,3] ;
      FONT oFontMain TEXT "HbRecorder" COORS 20 BTN_CLOSE BTN_MINIMIZE

   oPaneHea:SetSysbtnColor( arrColors[nTheme,6], arrColors[nTheme,1] )

   @ 0, HEA_HEIGHT PANEL oPaneTop SIZE oMain:nWidth, PL_HEIGHT ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   oRecord := HRecorder():New( oPaneTop, arrColors[nTheme] )

   ACTIVATE WINDOW oMain

   HRecorder():KillDevice()

   RETURN Nil