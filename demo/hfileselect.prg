/*
 * Audio player demo program, HFileSelect class - file selection dialog implementation
 * Harbour + HwGUI + Miniaudio
 *
 * HbAudio - Harbour wrappers for miniaudio
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
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

   CLASS VAR cCurrPath SHARED
   CLASS VAR aRecent SHARED  INIT {}
   DATA oDlg
   DATA lToSave   INIT .F.
   DATA lRecent   INIT .T.
   DATA cFileHis
   DATA aColors
   DATA aFilters
   DATA oStyleNormal, oStylePressed, oStyleOver
   DATA nPathPos

   METHOD New( aFilters, cCurrPath, aColors )
   METHOD Show()
   METHOD Open( aFilters, cCurrPath, aColors )
   METHOD Save( aFilters, cCurrPath, aColors )
   METHOD AddRecent( cFile )

ENDCLASS

METHOD New( aFilters, cCurrPath, aColors ) CLASS HFileSelect

   ::aFilters := Iif( Empty( aFilters ), { {"All Files","*"} }, aFilters )
   ::aColors := Iif( Empty(aColors), ;
      {CLR_BGRAY1,CLR_BGRAY2,CLR_DBROWN,CLR_GBROWN,CLR_BLACK,CLR_WHITE}, aColors )
   IF !Empty( cCurrPath )
      ::cCurrPath := cCurrPath
   ENDIF

   ::oStyleNormal := HStyle():New( {::aColors[CLR_BOARD]}, 1 )
   ::oStylePressed := HStyle():New( {::aColors[CLR_STYLE]}, 1,, 2, ::aColors[CLR_BTN1] )
   ::oStyleOver := HStyle():New( {::aColors[CLR_STYLE]}, 1 )

   RETURN Self

METHOD Show() CLASS HFileSelect

   LOCAL oDlg, oPaneHea, oPaneTop, oPaneF, oBrw1, oBrw2, oCombo, oEdit
   LOCAL oFont := HWindow():Getmain():oFont, cRes := "", nPaneFHeight := 4
   LOCAL lSelect := .F.
   LOCAL bEnter1 := {||
      LOCAL xPath := "", i
      xPath := oBrw1:aArray[oBrw1:nCurrent,2]
      IF Valtype( xPath ) == "A"
         oBrw2:aArray := SetBrw3( Self, xPath )
      ELSE
         ::cCurrPath := xPath
         IF !( Right( ::cCurrPath,1 ) $ "/\" )
            ::cCurrPath += hb_ps()
         ENDIF
         oBrw2:aArray := SetBrw2( Self, oCombo:Value )
      ENDIF
      oBrw2:Top()
      oBrw2:Refresh()

      RETURN .T.
   }
   LOCAL bEnter2 := {||
      IF oBrw2:nCurrent > 0 .AND. oBrw2:nCurrent <= oBrw2:nRecords
         IF 'D' $ oBrw2:aArray[oBrw2:nCurrent,5] .AND. ( !lSelect .OR. Empty(oEdit:GetText()) )
            ::cCurrPath += oBrw2:aArray[oBrw2:nCurrent,1] + hb_ps()
            oBrw2:aArray := SetBrw2( Self, oCombo:Value )
            oBrw2:Top()
            oBrw2:Refresh()
            oBrw1:aArray := SetBrw1( Self, ::cCurrPath )
            oBrw1:nCurrent := oBrw1:rowPos := ::nPathPos
            oBrw1:Refresh()
         ELSE
            IF ::lToSave
               cRes := ::cCurrPath + oEdit:GetText()
            ELSEIF oBrw2:aArray[oBrw2:nCurrent,5] == "@"
               cRes := oBrw2:aArray[oBrw2:nCurrent,6]
            ELSE
               cRes := ::cCurrPath + oBrw2:aArray[oBrw2:nCurrent,1]
               IF ::lRecent
                  ::AddRecent( cRes )
               ENDIF
            ENDIF
            hwg_EndDialog()
         ENDIF
      ELSEIF ::lToSave .AND. !Empty( oEdit:GetText() )
         cRes := ::cCurrPath + oEdit:GetText()
         hwg_EndDialog()
      ENDIF
      lSelect := .F.
      RETURN .T.
   }
   LOCAL bSelect := {||
      lSelect := .T.
      RETURN Eval( bEnter2 )
   }
   LOCAL bCombo := {||
      oBrw2:aArray := SetBrw2( Self, oCombo:Value )
      oBrw2:Top()
      oBrw2:Refresh()
      RETURN .T.
   }
   LOCAL bPosChg2 := {||
      IF oBrw2:nCurrent > 0 .AND. oBrw2:nCurrent <= oBrw2:nRecords .AND. ;
         !( 'D' $ oBrw2:aArray[oBrw2:nCurrent,5] )
         oEdit:SetText( oBrw2:aArray[oBrw2:nCurrent,1] )
      ENDIF
      RETURN .T.
   }

   IF ::lToSave
      ::lRecent := .F.
      nPaneFHeight := 30
   ELSEIF Empty(::aRecent) .AND. !Empty(::cFileHis)
      ::aRecent := LoadHis( ::cFileHis )
   ENDIF

   IF Empty( ::cCurrPath )
      ::cCurrPath := hb_DirBase()
   ENDIF
   IF !( Right( ::cCurrPath,1 ) $ "/\" )
      ::cCurrPath += hb_ps()
   ENDIF

   INIT DIALOG oDlg TITLE "" BACKCOLOR ::aColors[CLR_DLG] FONT oFont ;
      AT 100, 100 SIZE 700, 600 STYLE WND_NOTITLE

   ::oDlg := oDlg

   ADD HEADER PANEL oPaneHea HEIGHT HEA_HEIGHT TEXTCOLOR ::aColors[CLR_BTN2] ;
      BACKCOLOR ::aColors[CLR_HEAD] FONT oFont TEXT "File selection" COORS 20 BTN_CLOSE
   oPaneHea:SetSysbtnColor( ::aColors[CLR_BTN2], ::aColors[CLR_BOARD] )

   @ 0, oPaneHea:nHeight PANEL oPaneTop SIZE oDlg:nWidth, 4 ;
      BACKCOLOR ::aColors[CLR_STYLE] ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS

   @ 0, oPaneHea:nHeight+oPaneTop:nHeight BROWSE oBrw1 ARRAY ;
      SIZE 180, oDlg:nHeight-oPaneHea:nHeight-oPaneTop:nHeight-BOTTOM_HEIGHT-nPaneFHeight ;
      ON SIZE ANCHOR_TOPABS + ANCHOR_BOTTOMABS NO VSCROLL
   oBrw1:aArray := SetBrw1( Self, ::cCurrPath )
   oBrw1:bPosChanged := bEnter1
   oBrw1:bColor := ::aColors[CLR_STYLE]
   oBrw1:bColorSel := oBrw1:htbColor := ::aColors[CLR_BOARD]
   oBrw1:tColor := oBrw1:tColorSel := ::aColors[CLR_BTN2]
   oBrw1:lDispHead := oBrw1:lDispSep := .F.

   oBrw1:AddColumn( HColumn():New( "",{|v,o|o:aArray[o:nCurrent,1]},"C",32,0 ) )
   oBrw1:nCurrent := oBrw1:rowPos := ::nPathPos

   @ oBrw1:nWidth+4, oBrw1:nTop BROWSE oBrw2 ARRAY ;
      SIZE oDlg:nWidth-oBrw1:nWidth-4, oBrw1:nHeight ;
      ON SIZE ANCHOR_TOPABS + ANCHOR_BOTTOMABS + ANCHOR_RIGHTABS
   oBrw2:aArray := SetBrw2( Self, 1 )
   oBrw2:bEnter := bEnter2
   oBrw2:bColor := ::aColors[CLR_STYLE]
   oBrw2:bColorSel := oBrw2:htbColor := ::aColors[CLR_BOARD]
   oBrw2:tColor := oBrw2:tColorSel := ::aColors[CLR_BTN2]
   oBrw2:lDispHead := oBrw2:lDispSep := .F.

   oBrw2:AddColumn( HColumn():New( "",{|v,o|Iif('D' $ o:aArray[o:nCurrent,5],"<Dir>",FSize(o:aArray[o:nCurrent,2]))},"C",8,0,,, DT_RIGHT ) )
   oBrw2:AddColumn( HColumn():New( "",{|v,o|o:aArray[o:nCurrent,1]},"C",64,0 ) )

   @ @ oBrw1:nWidth, oBrw1:nTop SPLITTER SIZE 4,oBrw1:nHeight DIVIDE {oBrw1} FROM {oBrw2}

   @ 0, oBrw1:nTop+oBrw1:nHeight PANEL oPaneF SIZE oDlg:nWidth, nPaneFHeight ;
      BACKCOLOR ::aColors[CLR_STYLE] ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS
   IF ::lToSave
      @ 40,2 EDITBOX oEdit CAPTION "" OF oPaneF SIZE oDlg:nWidth-80,26 ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS
      oBrw2:bPosChanged := bPosChg2
   ENDIF

   @ oBrw1:nWidth+4,oDlg:nHeight-48 COMBOBOX oCombo ITEMS ::aFilters SIZE 240, 28 DISPLAYCOUNT 3 ON CHANGE bCombo

   @ oDlg:nWidth-240, oDlg:nHeight-48 OWNERBUTTON SIZE 100,30 TEXT "Cancel" ;
      COLOR ::aColors[CLR_BTN1] HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK {||hwg_EndDialog()}

   @ oDlg:nWidth-120, oDlg:nHeight-48 OWNERBUTTON SIZE 100,30 TEXT "Select" ;
      COLOR ::aColors[CLR_BTN1] HSTYLES ::oStyleNormal, ::oStylePressed, ::oStyleOver ;
      ON SIZE ANCHOR_LEFTABS + ANCHOR_RIGHTABS + ANCHOR_BOTTOMABS ;
      ON CLICK bSelect

   ACTIVATE DIALOG oDlg

   RETURN cRes

METHOD Open( aFilters, cCurrPath, aColors ) CLASS HFileSelect

   LOCAL o := ::New( aFilters, cCurrPath, aColors )

   RETURN o:Show()

METHOD Save( aFilters, cCurrPath, aColors ) CLASS HFileSelect

   LOCAL o := ::New( aFilters, cCurrPath, aColors )

   o:lToSave := .T.

   RETURN o:Show()

METHOD AddRecent( cFile ) CLASS HFileSelect

   hb_AIns( ::aRecent, 1, cFile, .T. )
   RETURN Nil

STATIC FUNCTION SetBrw1( o, cPath )

   LOCAL arr, arr2, i, cDrives

   IF '\' $ cPath
      cPath := StrTran( cPath, '\', '/' )
   ENDIF
   IF Right( cPath,1 ) == '/'
      cPath := hb_strShrink( cPath,1 )
   ENDIF
   IF Left( cPath,1 ) == '/'
      cPath := Substr( cPath,2 )
   ENDIF
   arr := hb_ATokens( cPath, '/' )
   IF Empty( arr[1] )
      arr[1] := '/'
   ENDIF

   arr2 := Array( Len( arr ),2 )
   FOR i := 1 TO Len( arr )
      arr2[Len(arr)-i+1,1] := arr[i]
   NEXT
   cPath := ""
   FOR i := Len( arr2 ) TO 1 STEP -1
      cPath += Iif( ':' $ arr2[i,1], "", hb_ps() ) + arr2[i,1]
      arr2[i,2] := cPath
      //hwg_writelog( arr2[i,1] + "  " + arr2[i,2] )
   NEXT

   o:nPathPos := 1
#ifdef __PLATFORM__UNIX
   hb_AIns( arr2, 1 , {"== Home",hb_getenv("HOME")}, .T. )
   hb_AIns( arr2, 1 , {"== Root","/"}, .T. )
   o:nPathPos += 2
#else
   cDrives := _getdrives()
#endif
   IF o:lRecent
      hb_AIns( arr2, 1 , {"== Recent",o:aRecent}, .T. )
      o:nPathPos ++
   ENDIF

   RETURN arr2

STATIC FUNCTION SetBrw2( o, nItem )

   LOCAL aDir, i, j, n1 := 0, af, lf

   aDir := Directory( o:cCurrPath+"*", "HSD" )
   af := hb_ATokens( o:aFilters[nItem,2], ';' )
   //hwg_Writelog( o:cCurrPath+"*" + " " + str(len(adir)) )

   FOR i := 1 TO Len( aDir )
      IF Empty( aDir[i] )
         LOOP
      ELSEIF aDir[i,1] == "." .OR. aDir[i,1] == ".."
         ADel( aDir, i )
         i --
         n1++
      ELSEIF "D" $ aDir[i,5]
         aDir[i,1] := " " + aDir[i,1]
      ELSE
         lf := .F.
         FOR j := 1 TO Len( af )
            IF hb_FileMatch( aDir[i,1], af[j] )
               lf := .T.
               EXIT
            ENDIF
         NEXT
         IF !lf
            ADel( aDir, i )
            i --
            n1++
         ENDIF
      ENDIF
   NEXT
   IF n1 > 0
      aDir := ASize( aDir, Len(aDir)-n1 )
   ENDIF

   aDir := ASort( aDir,,, {|z,y|Lower(z[1]) < Lower(y[1])} )
   FOR i := 1 TO Len( aDir )
      IF "D" $ aDir[i,5]
         IF Left( aDir[i,1],1 ) == " "
            aDir[i,1] := Substr( aDir[i,1],2 )
         ENDIF
      ENDIF
   NEXT

   RETURN aDir

STATIC FUNCTION SetBrw3( o, arr )

   LOCAL aDir := Array( Len(arr), 6 ), i

   FOR i := 1 TO Len( aDir )
      aDir[i,1] := hb_fnameNameExt( arr[i] )
      aDir[i,2] := 0
      aDir[i,5] := "@"
      aDir[i,6] := arr[i]
   NEXT

   RETURN aDir

STATIC FUNCTION LoadHis( cFile )

   LOCAL arr, cBuf := MemoRead( cFile ), i

   IF Empty( cBuf )
      RETURN {}
   ENDIF
   IF Chr(13) $ cBuf
      cBuf := StrTran( cBuf, Chr(13), "" )
   ENDIF
   arr := hb_aTokens( cBuf, Chr(10) )
   FOR i := Len( arr ) TO 1 STEP -1
      hb_ADel( arr, i, .T. )
   NEXT

   RETURN arr

STATIC FUNCTION FSize( n )

   RETURN Iif( n==0, "", Iif( n<=999999, PAdl(Ltrim(Str(n)),6), ;
      Iif( n<10238976,PAdl(Left(Ltrim(Str(Round(n/1024,1))),5)+"K",6), ;
      Iif( n<10484711424, PAdl(Left(Ltrim(Str(Round(n/1048576,1))),5)+"M",6), ;
      PAdl(Left(Ltrim(Str(Round(n/1073741824,1))),5)+"G",6) ) ) ) )

#pragma BEGINDUMP

#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
#else
   #define  UNICODE
   #include <windows.h>
   #include "hbapi.h"

   HB_FUNC( _GETDRIVES )
   {
      DWORD dwRes = GetLogicalDrives();
      int i;
      char buf[26], *ptr;

      if( dwRes )
      {
         ptr = buf;
         for( i = 0; i<26; i++ )
            if( (1 << i) & dwRes )
               *ptr++ = (char) i + 65;
         hb_retclen( buf, ptr-buf );
      }
   }
   /*
     0  The drive type cannot be determined.
     1  The root path is invalid; for example, there is no volume mounted at the specified path.
     2  The drive has removable media; for example, a floppy drive, thumb drive, or flash card reader.
     3  The drive has fixed media; for example, a hard disk drive or flash drive.
     4  The drive is a remote (network) drive.
     5  The drive is a CD-ROM drive.
     6  The drive is a RAM disk.
    */
   HB_FUNC( _GETDRIVETYPE )
   {
      int i;

   #ifdef UNICODE
      TCHAR wc1[6];
      MultiByteToWideChar( GetACP(), 0, hb_parc(1), -1, wc1, 4 );
      i = (int) GetDriveType( wc1 );
   #else
      i = (int) GetDriveType( hb_parc(1) );
   #endif
      hb_retni( i );
   }

#endif

#pragma ENDDUMP