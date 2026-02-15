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
      BACKCOLOR CLR_GBROWN FONT oFont STYLE WND_NOTITLE + WND_NOSIZEBOX ;
      ON EXIT {||oPlayer:KillSound()}

   ADD HEADER PANEL oPaneHea HEIGHT HEA_HEIGHT TEXTCOLOR CLR_WHITE BACKCOLOR CLR_DBROWN ;
      FONT oFont TEXT "HbPlayer" COORS 20 BTN_CLOSE BTN_MINIMIZE

   oPaneHea:SetSysbtnColor( CLR_WHITE, CLR_BGRAY1 )

   @ 0, HEA_HEIGHT PANEL oPaneTop SIZE oMain:nWidth, PL_HEIGHT HSTYLE oStyleNormal ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   oPlayer := HPlayer():New( oPaneTop, oPaneHea )

   ACTIVATE WINDOW oMain ON ACTIVATE {|| Iif(!Empty(cFile), oPlayer:PlayFile(cFile), .T.) }

   IF !Empty( HPlayer():pEngine )
      ma_Engine_UnInit( HPlayer():pEngine )
      HPlayer():pEngine := Nil
   ENDIF

   RETURN Nil

CLASS HPlayer

   CLASS VAR pEngine SHARED

   DATA pSound, cFile
   DATA oPane, oWnd, oBtnPlay, oTrack
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

   LOCAL bTrack := {|o|
      IF ::pSound != Nil
         //hwg_Writelog( "Set "+str(o:Value) )
         ma_sound_seek_to_pcm_frame( ::pSound, Int( o:Value*::nFramesAll ) )
         //hwg_Writelog( "Res: "+str(ma_sound_get_cursor_in_pcm_frames( ::pSound )) )
      ENDIF

      RETURN .T.
   }

   ::oPane := oPane
   ::oWnd := oWnd

   ::oStyleNormal := HStyle():New( {CLR_BGRAY1,CLR_BGRAY2}, 1 )
   ::oStylePressed := HStyle():New( {CLR_BGRAY1}, 1,, 2, CLR_BLACK )
   ::oStyleOver := HStyle():New( {CLR_BGRAY1}, 1 )

   @ 24, 8 OWNERBUTTON ::oBtnPlay OF oPane SIZE 24, 16 ;
      HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
      TEXT ">" COLOR CLR_WHITE ON CLICK { ||.t. }

   ::oTrack := HTrack():New( ::oPane,, 60, 8, ::oPane:nWidth-68, 16, ;
    ,, CLR_WHITE, CLR_BGRAY1, 20,, ;
       HStyle():New( { 0 }, 1, {8,8,8,8} ) )
   //::oTrack:bChange := bChange
   ::oTrack:Value := 0
   ::oTrack:oDrawn:bEndDrag := bTrack

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

   LOCAL nPos, nSec := Seconds()

   ::oBtnPlay:bClick := {||::Stop()}
   ::oBtnPlay:title := 'x'
   ::oBtnPlay:Refresh()

   ma_sound_start( ::pSound )
   ::lStopped := .F.
   ::nPlayPos := 0

   DO WHILE ::pSound != Nil .AND. ma_sound_is_playing( ::pSound )
      IF Seconds() - nSec > 0.2
         nSec := Seconds()
         IF ( ( nPos := ma_sound_get_cursor_in_pcm_frames( ::pSound ) ) - ::nPlayPos ) / ::nFramesAll > 0.005
            ::nPlayPos := nPos

            //IF nPlayPos >= nCurrPos + nDataLen*nZoom
               ::oTrack:Value := ::nPlayPos / ::nFramesAll
               //hwg_Writelog( "- "+str(::oTrack:Value)+" "+str(::nPlayPos) )
               ::oTrack:Refresh()
               //GraphScroll( 1 )
            //ELSE
               //HWindow():GetMain():oPaneGHea:Refresh()
            //ENDIF

         ENDIF
      ENDIF
      hwg_ProcessMessage()
      hb_gcStep()
      hwg_Sleep( 1 )
   ENDDO

   IF !::lStopped
      ::Stop()
      ::nPlayPos := 0
   ENDIF
   //GraphScroll( 0 )

   RETURN Nil

METHOD Stop() CLASS HPlayer

   ma_sound_stop( ::pSound )
   ::lStopped := .T.
   ::oBtnPlay:bClick := {||::Play()}
   ::oBtnPlay:title := '>'
   ::oBtnPlay:Refresh()

   RETURN Nil

METHOD KillSound() CLASS HPlayer

   IF !Empty( ::pSound )
      IF ma_sound_is_playing( ::pSound )
         ma_sound_stop( ::pSound )
      ENDIF
      ma_sound_uninit( ::pSound )
      ::pSound := Nil
   ENDIF

   RETURN .T.

METHOD End() CLASS HPlayer

   ::KillSound()

   RETURN .T.

EXIT PROCEDURE PExit

   oPlayer:End()
   IF !Empty( HPlayer():pEngine )
      ma_Engine_UnInit( HPlayer():pEngine )
      HPlayer():pEngine := Nil
   ENDIF

   RETURN