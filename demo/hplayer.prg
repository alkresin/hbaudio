/*
  */

#include "hbclass.ch"
#include "hwgui.ch"

#define CLR_BOARD   1          // oStyleNormal, Board, buttons, tracker
#define CLR_STYLE   2          // oStylePressed, oStyleOver
#define CLR_HEAD    3          // Dialog header pane
#define CLR_DLG     4          // Dialog back color
#define CLR_BTN1    5
#define CLR_BTN2    6

CLASS HPlayer

   CLASS VAR pEngine SHARED

   DATA pSound, cFile
   DATA oBoard, oWnd, oBtnAdd, oBtnPlay, oBtnVol, oSayTime, oTrack
   DATA aColors                //
   DATA oBrushBtn1, oBrushBtn2
   DATA oStyleNormal, oStylePressed, oStyleOver
   DATA lStopped  INIT .T.
   DATA nPlayPos
   DATA nChannels, nRate, nFramesAll
   DATA cLastPath

   METHOD New( oPane, oWnd, aColors )
   METHOD PlayFile( cFile )
   METHOD Play()
   METHOD Stop()
   METHOD Volume()
   METHOD ShowTime( n )
   METHOD KillSound()
   METHOD End()

ENDCLASS

METHOD New( oPane, oWnd, aColors, cLastPath, nVolume ) CLASS HPlayer

   LOCAL bTrack := {|o|
      IF ::pSound != Nil
         ma_sound_seek_to_pcm_frame( ::pSound, Int( o:Value*::nFramesAll ) )
         ::ShowTime( Int( o:Value*::nFramesAll ) )
      ENDIF

      RETURN .T.
   }
   LOCAL bPaintB1 := {|o,hdc|
      LOCAL nl := o:nLeft, nt := o:nTop, oStyle
      oStyle := Iif( Len(o:aStyles) > o:nState, o:aStyles[o:nState + 1], ATail(o:aStyles) )
      oStyle:Draw( hDC, o:nLeft, o:nTop, o:nLeft+o:nWidth-1, o:nTop+o:nHeight-1 )
      IF ::lStopped
         hwg_Triangle_Filled( hDC, nl+4, nt+6, nl+o:nWidth-4, nt+6+Int((o:nHeight-12)/2), nl+4, nt+o:nHeight-6, .F., ::oBrushBtn1:handle )
      ELSE
         hwg_Rectangle_Filled( hDC, nl+4, nt+6, nl+8, nt+o:nHeight-6, .F., ::oBrushBtn1:handle )
         hwg_Rectangle_Filled( hDC, nl+o:nWidth-8, nt+6, nl+o:nWidth-4, nt+o:nHeight-6, .F., ::oBrushBtn1:handle )
      ENDIF
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

   ::cLastPath := Iif( Empty( cLastPath ), hb_DirBase(), cLastPath )
   ::oWnd := oWnd
   ::aColors := aColors

   ::oBrushBtn1 := HBrush():Add( ::aColors[CLR_BTN1] )
   ::oBrushBtn2 := HBrush():Add( ::aColors[CLR_BTN2] )
   ::oStyleNormal := HStyle():New( {::aColors[CLR_BOARD]}, 1 )
   ::oStylePressed := HStyle():New( {::aColors[CLR_STYLE]}, 1,, 2, ::aColors[CLR_BTN1] )
   ::oStyleOver := HStyle():New( {::aColors[CLR_STYLE]}, 1 )

   @ 0, 0 BOARD ::oBoard SIZE oPane:nWidth, oPane:nHeight OF oPane BACKCOLOR ::aColors[CLR_BOARD] ;
      ON SIZE ANCHOR_LEFTABS+ANCHOR_RIGHTABS

   @ 2, 2 DRAWN ::oBtnAdd SIZE 20, 28 ;
      HSTYLES { ::oStyleNormal, ::oStyleOver, ::oStyleNormal }
   ::oBtnAdd:cTooltip := "Open file"
   ::oBtnAdd:bPaint := bPaintB2
   ::oBtnAdd:bClick := {|| ::PlayFile("") }

   @ 30, 2 DRAWN ::oBtnPlay SIZE 20, 28 ;
      HSTYLES { ::oStyleNormal, ::oStyleOver, ::oStyleNormal }
   ::oBtnPlay:bPaint := bPaintB1

   @ 60, 8 DRAWN TRACK ::oTrack SIZE ::oBoard:nWidth-220, 16 COLOR ::aColors[CLR_BTN2] BACKCOLOR ::aColors[CLR_BOARD] ;
      SLIDER SIZE 20 SLIDER HSTYLE HStyle():New( { 0 }, 1, {8,8,8,8} ) AXIS

   @ ::oBoard:nWidth-150, 2 DRAWN ::oSayTime SIZE 120, 28 COLOR ::aColors[CLR_BTN2] BACKCOLOR ::aColors[CLR_BOARD]

   @ ::oBoard:nWidth-28, 2 DRAWN ::oBtnVol SIZE 20, 28 COLOR ::aColors[CLR_BTN1] TEXT 'V';
      HSTYLES { ::oStyleNormal, ::oStyleOver, ::oStyleNormal }
   //::oBtnVol:cTooltip := "Volume"
   ::oBtnVol:bClick := {|| ::Volume() }

   ::oTrack:Value := 0
   ::oTrack:bEndDrag := bTrack

   IF Empty( ::pEngine )
     ::pEngine := ma_Engine_Init()
     ma_engine_set_volume( ::pEngine, Iif( Empty( nVolume ), 0.5, nVolume ) )
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
      ::cFile := hwg_SelectfileEx( , ::cLastPath, { { "All files", "*.*" } } )
#else
      ::cFile := hwg_Selectfile( { "All files" }, { "*.*" }, ::cLastPath  )
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
      ::cLastPath := hb_fnameDir( ::cFile )

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

   ::ShowTime( ::nPlayPos := ma_sound_get_cursor_in_pcm_frames( ::pSound ) )
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
            ::ShowTime( ::nPlayPos )

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

METHOD ShowTime( n ) CLASS HPlayer

   LOCAL cTime

   n := Int( n/::nRate )
   cTime := Ltrim(Str(Int(n/60))) + ":" + PAdl(Ltrim(Str(Int(n%60))),2,'0') + "/"
   n := Int( ::nFramesAll/::nRate )
   cTime += Ltrim(Str(Int(n/60))) + ":" + PAdl(Ltrim(Str(Int(n%60))),2,'0')
   ::oSayTime:SetText( cTime )

   RETURN Nil

METHOD Volume() CLASS HPlayer

   LOCAL oDlg, oPaneHea, oTrack, oFont := HWindow():Getmain():oFont
   LOCAL nVol := ma_engine_get_volume( ::pEngine )
   LOCAL bVolChange := {|o|
      ma_engine_set_volume( ::pEngine, o:Value*2 )
      RETURN .T.
   }

   INIT DIALOG oDlg TITLE "" BACKCOLOR ::aColors[CLR_DLG] FONT oFont ;
      AT 300, 58 SIZE 260, 120 STYLE WND_NOTITLE

   ADD HEADER PANEL oPaneHea HEIGHT 32 TEXTCOLOR ::aColors[CLR_BTN2] BACKCOLOR ::aColors[CLR_HEAD] ;
      FONT oFont TEXT "Volume" COORS 20 BTN_CLOSE
   oPaneHea:SetSysbtnColor( ::aColors[CLR_BTN2], ::aColors[CLR_BOARD] )

   @ 20, 40 TRACK oTrack SIZE 220, 20 COLOR ::aColors[CLR_BTN2] BACKCOLOR ::aColors[CLR_BOARD] ;
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