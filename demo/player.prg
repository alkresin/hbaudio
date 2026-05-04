/*
 * Audio player demo program, main source file
 * Harbour + HwGUI + Miniaudio
 *
 * HbAudio - Harbour wrappers for miniaudio
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "fileio.ch"
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

REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866, HB_CODEPAGE_UTF8

STATIC hlock := -1, lLock := .F., cFlockName, oTimer
STATIC oPlayer
STATIC arrColors := { {CLR_BGRAY1,CLR_BGRAY2,CLR_DBROWN,CLR_GBROWN,CLR_BLACK,CLR_WHITE}, ;
   {0x797979,0x555555,0x222222,0x353535,CLR_BLACK,CLR_WHITE} }
STATIC nTheme := 1, cLastPath, oFontMain, nVolume := 0.5, lTime := .T., lGraph := .F., cFileHis, nMaxHis := 40

FUNCTION Main( cFile )

   LOCAL oMain, oPaneHea, oPaneTop

   hb_cdpSelect( "RU1251" )

   /* IF !Empty( cFile )
      cFile := cpConvert( cFile )
   ENDIF */
   IF !onStart( cFile )
      RETURN Nil
   ENDIF

   ReadIni()

   IF Empty( oFontMain )
      PREPARE FONT oFontMain NAME "Georgia" WIDTH 0 HEIGHT - 15 ITALIC CHARSET 4
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

   SET TIMER oTimer OF oMain VALUE 500 ACTION {||CheckLock()}

   SET KEY 0, VK_SPACE TO Iif( oPlayer:lStopped, oPlayer:Play(), oPlayer:Stop() )

   ACTIVATE WINDOW oMain ON ACTIVATE {|| Iif(!Empty(cFile), oPlayer:PlayFile(cFile), .T.) }

   IF !Empty( HPlayer():pEngine )
      ma_Engine_UnInit( HPlayer():pEngine )
      HPlayer():pEngine := Nil
   ENDIF

   SaveHis()

   RETURN Nil

STATIC FUNCTION onStart( cFile )

   LOCAL l
   cFlockName := hb_dirTemp() + "player.glock"

   IF File( cFlockName )
#ifdef __PLATFORM__UNIX
      l := ( _isprocessexists( hb_fnameNameExt( hb_Argv(0) ) ) > 0 )
#else
      l := ( FErase( cFlockName ) != 0 )
#endif
      IF l
         IF !Empty( cFile )
            hlock := FOpen( cFlockName, FO_READWRITE + FO_SHARED )
            FWrite( hlock, " " + cFile + "@", Len(cFile)+2 )
            FSeek( hlock, 0, 0 )
            FWrite( hlock, "@", 1 )
            FClose( hlock )
         ENDIF
         RETURN .F.
      ENDIF
   ENDIF

   hlock := FCreate( cFlockName )
   FClose( hlock )
   hlock := FOpen( cFlockName, FO_READWRITE + FO_SHARED )
   lLock := .T.

   RETURN .T.

STATIC FUNCTION CheckLock()

   LOCAL nRead, nPos, cFile
   STATIC cBuff := .F.

   IF Valtype( cBuff ) == "L"
      cBuff := Space( 256 )
   ENDIF
   FSeek( hlock, 0, 0 )
   nRead := FRead( hlock, @cBuff, 256 )
   IF Left( cBuff,1 ) == '@' .AND. ( nPos := hb_At( "@", cBuff, 2 ) ) > 0
      cFile := SubStr( cBuff, 2, nPos-2 )
      FSeek( hlock, 0, 0 )
      FWrite( hlock, "   ", 3 )
      oPlayer:PlayFile( cFile )
   ENDIF

   RETURN Nil

STATIC FUNCTION cpConvert( cFile )

   LOCAL i, n

   //hwg_writelog( cFile )
   FOR i := 1 TO Len( cFile )
      IF ( n := hb_bPeek( cFile, i ) ) >= 128 .AND. n <= 191
         //hwg_writelog( str(i) + ":" + str(n) )
         RETURN hb_translate( cFile, "RU866", "RU1251" )
      ENDIF
   NEXT

   RETURN cFile

STATIC FUNCTION SaveHis()

   LOCAL s := "[PLAYLIST]" + Chr(10), i, n

   IF !Empty( cFileHis ) .AND. !Empty( HFileSelect():aRecent )
      n := Min( Len( HFileSelect():aRecent ), nMaxHis )
      FOR i := 1 TO n
         s += HFileSelect():aRecent[i] + Chr(10)
      NEXT
      hb_MemoWrit( cFileHis, s )
   ENDIF

   RETURN Nil

STATIC FUNCTION ReadIni()

   LOCAL hIni := _IniRead( hb_Dirbase() + "player.ini" ), aIni, nSect, aSect, cTemp, aList, i

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
               IF hb_hHaskey( aSect, cTemp := "history_file" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  cFileHis := hb_Dirbase() + cTemp
               ENDIF
               IF hb_hHaskey( aSect, cTemp := "history_items" ) .AND. !Empty( cTemp := aSect[ cTemp ] )
                  nMaxHis := Val( cTemp )
               ENDIF
            ENDIF
         ENDIF
      NEXT
      IF !Empty( cFileHis ) .AND. File( cFileHis ) .AND. !Empty( cTemp := Memoread( cFileHis ) )

         aList := hb_aTokens( cTemp, Chr(10) )
         FOR i := 1 TO Len( aList )
            cTemp := Iif( Left( aList[i],1 ) == ' ', Ltrim( aList[i] ), aList[i] )
            IF Left( cTemp, 1 ) $ "[;#"
               LOOP
            ENDIF
            cTemp := Trim( Iif( Right(cTemp,1)==Chr(13), Left( cTemp,Len(cTemp)-1 ), cTemp ) )
            IF !Empty( cTemp ) .AND. File( cTemp )
               AAdd( HFileSelect():aRecent, cTemp )
            ENDIF
         NEXT
      ENDIF
   ENDIF

   RETURN Nil

EXIT PROCEDURE PExit

   IF !Empty( oPlayer )
      oPlayer:End()
      IF !Empty( HPlayer():pEngine )
         ma_Engine_UnInit( HPlayer():pEngine )
         HPlayer():pEngine := Nil
      ENDIF
   ENDIF
   IF lLock
      FClose( hLock )
      FErase( cFlockName )
   ENDIF

   RETURN

#pragma BEGINDUMP
#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
   #include <stdio.h>
   #include <stdlib.h>
   #include <string.h>
   #include <unistd.h>
   #include <dirent.h>
   #include <ctype.h>

   #include "hbapi.h"

   HB_FUNC( _ISPROCESSEXISTS )
   {
      const char* process_name = hb_parc(1);
      DIR* dir;
      struct dirent* entry;
      char path[280];
      char buffer[280];
      FILE* fp;
      int pid = -1;
      int current_pid = getpid();

      dir = opendir("/proc");
      if (!dir) {
         hb_retnl( -1 );
         return;
      }

      while ((entry = readdir(dir)) != NULL) {
         // Проверяем, что это директория с числовым именем (PID)
         if (!isdigit(entry->d_name[0])) continue;

         snprintf(path, sizeof(path), "/proc/%s/comm", entry->d_name);
         fp = fopen(path, "r");
         if (!fp) continue;

         if (fgets(buffer, sizeof(buffer), fp)) {
            // Убираем символ новой строки
            buffer[strcspn(buffer, "\n")] = 0;

            if (strcmp(buffer, process_name) == 0) {
               int found_pid = atoi(entry->d_name);
               if (found_pid != current_pid) {
                  pid = found_pid;
                  fclose(fp);
                  break;
               }
            }
         }
         fclose(fp);
      }

      closedir(dir);
      hb_retnl( pid );
   }
#endif
#pragma ENDDUMP