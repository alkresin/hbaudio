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

   ACTIVATE WINDOW oMain ON ACTIVATE {|| Iif(!Empty(cFile), oPlayer:PlayFile(cFile), .T.) }

   IF !Empty( HPlayer():pEngine )
      ma_Engine_UnInit( HPlayer():pEngine )
      HPlayer():pEngine := Nil
   ENDIF

   RETURN Nil

CLASS HPlayer

   CLASS VAR pEngine SHARED

   DATA pSound, cFile
   DATA oBoard, oWnd, oBtnAdd, oBtnPlay, oBtnVol, oSayTime, oTrack
   DATA oBrushBtn1, oBrushBtn2
   DATA oStyleNormal, oStylePressed, oStyleOver
   DATA lStopped  INIT .T.
   DATA nPlayPos
   DATA nChannels, nRate, nFramesAll

   METHOD New( oPane, oWnd )
   METHOD PlayFile( cFile )
   METHOD Play()
   METHOD Stop()
   METHOD Volume()
   METHOD KillSound()
   METHOD End()

ENDCLASS

METHOD New( oPane, oWnd ) CLASS HPlayer

   LOCAL bTrack := {|o|
      IF ::pSound != Nil
         ma_sound_seek_to_pcm_frame( ::pSound, Int( o:Value*::nFramesAll ) )
      ENDIF

      RETURN .T.
   }
   LOCAL bPaintB1 := {|o,hdc|
      LOCAL nl := o:nLeft, nt := o:nTop, oStyle
      oStyle := Iif( Len(o:aStyles) > o:nState, o:aStyles[o:nState + 1], ATail(o:aStyles) )
      oStyle:Draw( hDC, o:nLeft, o:nTop, o:nLeft+o:nWidth-1, o:nTop+o:nHeight-1 )
      IF ::lStopped
         hwg_Triangle_Filled( hDC, nl+4, nt+6, nl+o:nWidth-4, nt+6+Int((o:nHeight-12)/2), nl+4, nt+o:nHeight-6, .F., oPlayer:oBrushBtn1:handle )
      ELSE
         hwg_Rectangle_Filled( hDC, nl+4, nt+6, nl+8, nt+o:nHeight-6, .F., oPlayer:oBrushBtn1:handle )
         hwg_Rectangle_Filled( hDC, nl+o:nWidth-8, nt+6, nl+o:nWidth-4, nt+o:nHeight-6, .F., oPlayer:oBrushBtn1:handle )
      ENDIF
      RETURN 0
   }
   LOCAL bPaintB2 := {|o,hdc|
      LOCAL nl := o:nLeft, nt := o:nTop, oStyle
      oStyle := Iif( Len(o:aStyles) > o:nState, o:aStyles[o:nState + 1], ATail(o:aStyles) )
      oStyle:Draw( hDC, o:nLeft, o:nTop, o:nLeft+o:nWidth-1, o:nTop+o:nHeight-1 )
      hwg_Rectangle_Filled( hDC, nl+4+Int((o:nWidth-8)/2)-1, nt+6, nl+4+Int((o:nWidth-8)/2)+1, nt+o:nHeight-6, .F., oPlayer:oBrushBtn2:handle )
      hwg_Rectangle_Filled( hDC, nl+6, nt+4+Int((o:nHeight-8)/2)-1, nl+o:nWidth-4, nt+6+Int((o:nHeight-12)/2)+1, .F., oPlayer:oBrushBtn2:handle )
      RETURN 0
   }

   ::oWnd := oWnd

   ::oBrushBtn1 := HBrush():Add( CLR_BLACK )
   ::oBrushBtn2 := HBrush():Add( CLR_WHITE )
   ::oStyleNormal := HStyle():New( {CLR_BGRAY1}, 1 )
   ::oStylePressed := HStyle():New( {CLR_BGRAY2}, 1,, 2, CLR_BLACK )
   ::oStyleOver := HStyle():New( {CLR_BGRAY2}, 1 )

   @ 0, 0 BOARD ::oBoard SIZE oPane:nWidth, oPane:nHeight OF oPane BACKCOLOR CLR_BGRAY1 ;
      ON SIZE ANCHOR_LEFTABS+ANCHOR_RIGHTABS

   @ 2, 2 DRAWN ::oBtnAdd SIZE 20, 28 COLOR CLR_WHITE ;
      HSTYLES { ::oStyleNormal, ::oStyleOver, ::oStyleNormal }
   ::oBtnAdd:cTooltip := "Open file"
   ::oBtnAdd:bPaint := bPaintB2
   ::oBtnAdd:bClick := {|| oPlayer:PlayFile("") }

   @ 30, 2 DRAWN ::oBtnPlay SIZE 20, 28 COLOR CLR_WHITE ;
      HSTYLES { ::oStyleNormal, ::oStyleOver, ::oStyleNormal }
   ::oBtnPlay:bPaint := bPaintB1

   @ 60, 8 DRAWN TRACK ::oTrack SIZE ::oBoard:nWidth-220, 16 COLOR CLR_WHITE BACKCOLOR CLR_BGRAY1 ;
      SLIDER SIZE 20 SLIDER HSTYLE HStyle():New( { 0 }, 1, {8,8,8,8} ) AXIS

   @ ::oBoard:nWidth-150, 2 DRAWN ::oSayTime SIZE 120, 28 COLOR CLR_WHITE BACKCOLOR CLR_BGRAY1

   @ ::oBoard:nWidth-28, 2 DRAWN ::oBtnVol SIZE 20, 28 COLOR CLR_BLACK TEXT 'V';
      HSTYLES { ::oStyleNormal, ::oStyleOver, ::oStyleNormal }
   ::oBtnVol:cTooltip := "Volume"
   ::oBtnVol:bClick := {|| oPlayer:Volume() }

   ::oTrack:Value := 0
   ::oTrack:bEndDrag := bTrack

   IF Empty( ::pEngine )
     ::pEngine := ma_Engine_Init()
     ma_engine_set_volume( ::pEngine, 0.5 )
   ENDIF

   RETURN Self

METHOD PlayFile( cFile ) CLASS HPlayer

   LOCAL oTimer

   ::cFile := Nil

   IF !Empty( cFile )
      IF File( cFile )
         ::cFile := cFile
      ENDIF
   ELSEIF cFile == ""
#ifdef __PLATFORM__UNIX
      ::cFile := hwg_SelectfileEx( , hb_DirBase(), { { "All files", "*.*" } } )
#else
      ::cFile := hwg_Selectfile( { "All files" }, { "*.*" }, hb_DirBase()  )
#endif
   ENDIF

   IF !Empty( ::cFile )
      ::KillSound()
      IF !Empty( ::oWnd )
         ::oWnd:SetText( hb_fnameName( ::cFile ) )
      ENDIF
      IF Empty( ::pSound := ma_Sound_Init( ::pEngine, ::cFile ) )
         hwg_MsgStop( "ma_Sound_Init() failed" )
         ::cFile := Nil
         RETURN Nil
      ENDIF

      ::nChannels := ma_sound_get_channels( ::pSound )
      ::nRate := ma_engine_get_sample_rate( ::pEngine )
      ::nFramesAll := ma_sound_get_length_in_pcm_frames( ::pSound )

      SET TIMER oTimer OF ::oBoard VALUE 10 ACTION {||::Play()} ONCE
   ENDIF

   RETURN .T.

METHOD Play() CLASS HPlayer

   LOCAL nPos, nSec := Seconds(), n, cTime

   IF Empty( ::pSound )
      RETURN Nil
   ENDIF

   ::oBtnPlay:bClick := {||::Stop()}
   ::oBtnPlay:title := 'x'
   ::oBtnPlay:Refresh()

   ma_sound_start( ::pSound )
   ::lStopped := .F.

   ::nPlayPos := ma_sound_get_cursor_in_pcm_frames( ::pSound )
   n := Int( ::nPlayPos/::nRate )
   cTime := Ltrim(Str(Int(n/60))) + ":" + PAdl(Ltrim(Str(Int(n%60))),2,'0') + "/"
   n := Int( ::nFramesAll/::nRate )
   cTime += Ltrim(Str(Int(n/60))) + ":" + PAdl(Ltrim(Str(Int(n%60))),2,'0')
   ::oSayTime:SetText( cTime )

   DO WHILE ::pSound != Nil .AND. ma_sound_is_playing( ::pSound )
      IF Seconds() - nSec > 0.2
         nSec := Seconds()
         IF Abs( ( ( nPos := ma_sound_get_cursor_in_pcm_frames( ::pSound ) ) - ;
            ::nPlayPos ) / ::nFramesAll ) > 0.005
            ::nPlayPos := nPos

            ::oTrack:Value := ::nPlayPos / ::nFramesAll
            ::oTrack:Refresh()

            n := Int( ::nPlayPos/::nRate )
            cTime := Ltrim(Str(Int(n/60))) + ":" + PAdl(Ltrim(Str(Int(n%60))),2,'0') + "/"
            n := Int( ::nFramesAll/::nRate )
            cTime += Ltrim(Str(Int(n/60))) + ":" + PAdl(Ltrim(Str(Int(n%60))),2,'0')
            ::oSayTime:SetText( cTime )

         ENDIF
      ENDIF
      hwg_ProcessMessage()
      hb_gcStep()
      hwg_Sleep( 1 )
   ENDDO

   IF !::lStopped
      ::Stop()
   ENDIF

   RETURN Nil

METHOD Stop() CLASS HPlayer

   ma_sound_stop( ::pSound )
   ::lStopped := .T.
   ::oBtnPlay:bClick := {||::Play()}
   ::oBtnPlay:title := '>'
   ::oBtnPlay:Refresh()

   RETURN Nil

METHOD Volume() CLASS HPlayer

   LOCAL oDlg, oPaneHea, oTrack, oFont := HWindow():Getmain():oFont
   LOCAL nVol := ma_engine_get_volume( ::pEngine )
   LOCAL bVolChange := {|o|
      ma_engine_set_volume( ::pEngine, o:Value*2 )
      RETURN .T.
   }

   INIT DIALOG oDlg TITLE "" BACKCOLOR CLR_GBROWN FONT oFont ;
      AT 300, 58 SIZE 260, 120 STYLE WND_NOTITLE

   ADD HEADER PANEL oPaneHea HEIGHT 32 TEXTCOLOR CLR_WHITE BACKCOLOR CLR_DBROWN ;
      FONT oFont TEXT "Volume" COORS 20 BTN_CLOSE
   oPaneHea:SetSysbtnColor( CLR_WHITE, CLR_BGRAY1 )

   @ 20, 40 TRACK oTrack SIZE 220, 20 COLOR CLR_WHITE BACKCOLOR CLR_BGRAY1 ;
      SLIDER SIZE 20 SLIDER HSTYLE HStyle():New( { 0 }, 1, {8,8,8,8} ) AXIS
   oTrack:bChange := bVolChange
   oTrack:Value := nVol/2

   @ 80,80 OWNERBUTTON SIZE 100,30 TEXT "Close" COLOR 0xffffff ;
         HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
         ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
         ON CLICK {||hwg_EndDialog()}

   ACTIVATE DIALOG oDlg

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