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
#define CLR_GRAPH      65280

#define CLR_BROWN_1  0x154780
#define CLR_BROWN_2  0x6a9cd4
#define CLR_BROWN_3  0xaad2ff
#define CLR_BROWN_4  0x396eaa
#define CLR_BROWN_5  0x9dc7f6
#define CLR_DLGBACK  0x154780
#define CLR_DLGHEA   0x2F343F

#define HEA_HEIGHT         30
#define PL_WIDTH          600
#define PL_HEIGHT          36

STATIC oPlayer

FUNCTION Main( cFile )

   LOCAL oMain, oPaneHea, oPaneTop, oFont, oStyleNormal

   oStyleNormal := HStyle():New( {CLR_BGRAY1,CLR_BGRAY2}, 1 )

   PREPARE FONT oFont NAME "Georgia" WIDTH 0 HEIGHT - 17 ITALIC

   INIT WINDOW oMain MAIN TITLE "" AT 200, 0 SIZE PL_WIDTH, HEA_HEIGHT+PL_HEIGHT  ;
      BACKCOLOR CLR_GBROWN FONT oFont STYLE WND_NOTITLE + WND_NOSIZEBOX

   ADD HEADER PANEL oPaneHea HEIGHT HEA_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR CLR_DBROWN ;
      FONT oFont TEXT "HbPlayer" COORS 20 BTN_CLOSE BTN_MINIMIZE

   oPaneHea:SetSysbtnColor( CLR_WHITE, CLR_BGRAY1 )

   @ 0, HEA_HEIGHT PANEL oPaneTop SIZE oMain:nWidth, PL_HEIGHT HSTYLE oStyleNormal ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   oPlayer := HPlayer():New( oPaneTop, oPaneHea )

   ACTIVATE WINDOW oMain ON ACTIVATE {|| Iif(!Empty(cFile), oPlayer:PlayFile(cFile), .T.) }

   RETURN Nil

CLASS HPlayer

   CLASS VAR pEngine SHARED

   DATA pSound, cFile
   DATA oPane, oWnd, oBtnPlay
   DATA oStyleNormal, oStylePressed, oStyleOver
   DATA lStopped  INIT .T.
   DATA nPlayPos
   DATA nChannels, nRate, nFramesAll

   METHOD New( oPane, oWnd )
   METHOD PlayFile( cFile )
   METHOD Play()
   METHOD Stop()
   METHOD KillSound()
   METHOD End()

ENDCLASS

METHOD New( oPane, oWnd ) CLASS HPlayer

   ::oPane := oPane
   ::oWnd := oWnd

   ::oStyleNormal := HStyle():New( {CLR_BGRAY1,CLR_BGRAY2}, 1 )
   ::oStylePressed := HStyle():New( {CLR_BGRAY1}, 1,, 2, CLR_BLACK )
   ::oStyleOver := HStyle():New( {CLR_BGRAY1}, 1 )

   @ 24, 8 OWNERBUTTON ::oBtnPlay OF oPane SIZE 24, 24 ;
      HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
      TEXT ">" COLOR CLR_WHITE ON CLICK { ||.t. }

   IF Empty( ::pEngine )
     ::pEngine := ma_Engine_Init()
   ENDIF

   RETURN Self

METHOD PlayFile( cFile ) CLASS HPlayer

   LOCAL oTimer

   ::cFile := Nil
   ::KillSound()

   IF !Empty( cFile )
      IF File( cFile )
         ::cFile := cFile
         IF !Empty( ::oWnd )
            ::oWnd:SetText( hb_fnameName( ::cFile ) )
         ENDIF
      ENDIF
   ELSE
   ENDIF

   IF !Empty( ::cFile )
      IF Empty( ::pSound := ma_Sound_Init( ::pEngine, ::cFile ) )
         hwg_MsgStop( "ma_Sound_Init() failed" )
         ::cFile := Nil
         RETURN Nil
      ENDIF

      ::nChannels := ma_sound_get_channels( ::pSound )
      ::nRate := ma_engine_get_sample_rate( ::pEngine )
      ::nFramesAll := ma_sound_get_length_in_pcm_frames( ::pSound )

      SET TIMER oTimer OF ::oPane VALUE 10 ACTION {||::Play()} ONCE
   ENDIF

   RETURN Nil

METHOD Play() CLASS HPlayer

   LOCAL nPos

   ma_sound_start( ::pSound )
   ::nPlayPos := 0

   DO WHILE ::pSound != Nil .AND. ma_sound_is_playing( ::pSound )
      IF ( nPos := ma_sound_get_cursor_in_pcm_frames( ::pSound ) ) != ::nPlayPos
         ::nPlayPos := nPos
         /*
         IF nPlayPos >= nCurrPos + nDataLen*nZoom
            GraphScroll( 1 )
         ELSE
            HWindow():GetMain():oPaneGHea:Refresh()
         ENDIF
         */
      ENDIF
      hwg_ProcessMessage()
      hb_gcStep()
      hwg_Sleep( 20 )
   ENDDO

   ::lStoped := .T.
   ma_sound_stop( ::pSound )
   ::nPlayPos := 0
   //GraphScroll( 0 )

   RETURN Nil

METHOD Stop() CLASS HPlayer

   RETURN Nil

METHOD KillSound() CLASS HPlayer

   IF !Empty( ::pSound )
      IF ma_sound_is_playing( ::pSound )
         ma_sound_stop( ::pSound )
      ENDIF
      ma_sound_uninit( ::pSound )
      ::pSound := Nil
   ENDIF

   RETURN Nil

METHOD End() CLASS HPlayer

   hwg_writelog( "End" )
   ::KillSound()

   RETURN .T.

EXIT PROCEDURE PExit

   oPlayer:End()
   IF !Empty( HPlayer():pEngine )
      ma_Engine_UnInit( HPlayer():pEngine )
      HPlayer():pEngine := Nil
   ENDIF

   RETURN