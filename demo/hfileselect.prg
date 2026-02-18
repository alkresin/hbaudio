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
   DATA lToSave   INIT .F.
   DATA aColors
   DATA aFilters
   DATA cCurrPath
   DATA oStyleNormal, oStylePressed, oStyleOver

   METHOD New( aFilters, cCurrPath, aColors )
   METHOD Show()
   METHOD Open( aFilters, cCurrPath, aColors )
   METHOD Save( aFilters, cCurrPath, aColors )

ENDCLASS

METHOD New( aFilters, cCurrPath, aColors ) CLASS HFileSelect

   ::aFilters := Iif( Empty( aFilters ), { {"All Files","*"} }, aFilters )
   ::aColors := Iif( Empty(aColors), ;
      {CLR_BGRAY1,CLR_BGRAY2,CLR_DBROWN,CLR_GBROWN,CLR_BLACK,CLR_WHITE}, aColors )
   ::cCurrPath := Iif( Empty(cCurrPath), hb_DirBase(), cCurrPath )
   IF !( Right( ::cCurrPath,1 ) $ "/\" )
      ::cCurrPath += hb_ps()
   ENDIF

   ::oStyleNormal := HStyle():New( {::aColors[CLR_BOARD]}, 1 )
   ::oStylePressed := HStyle():New( {::aColors[CLR_STYLE]}, 1,, 2, ::aColors[CLR_BTN1] )
   ::oStyleOver := HStyle():New( {::aColors[CLR_STYLE]}, 1 )

   RETURN Self

METHOD Show() CLASS HFileSelect

   LOCAL oDlg, oPaneHea, oPaneTop, oPaneF, oBrw1, oBrw2, oCombo, oEdit
   LOCAL oFont := HWindow():Getmain():oFont, cRes := "", nPaneFHeight := 4
   LOCAL bEnter1 := {||
      LOCAL cPath := "", i
      FOR i := Len(oBrw1:aArray) TO oBrw1:nCurrent STEP -1
         cPath += Iif( oBrw1:aArray[i]=='/', "", oBrw1:aArray[i] ) + hb_ps()
      NEXT
      ::cCurrPath := cPath
      oBrw2:aArray := SetBrw2( Self, oCombo:Value )
      oBrw2:Top()
      oBrw2:Refresh()

      RETURN .T.
   }
   LOCAL bEnter2 := {||
      IF 'D' $ oBrw2:aArray[oBrw2:nCurrent,5]
         ::cCurrPath += oBrw2:aArray[oBrw2:nCurrent,1] + hb_ps()
         oBrw2:aArray := SetBrw2( Self, oCombo:Value )
         oBrw2:Top()
         oBrw2:Refresh()
         oBrw1:aArray := SetBrw1( Self, ::cCurrPath )
         oBrw1:Top()
         oBrw1:Refresh()
      ELSE
         IF ::lToSave
            cRes := oEdit:GetText()
         ELSE
            cRes := ::cCurrPath + oBrw2:aArray[oBrw2:nCurrent,1]
         ENDIF
         hwg_EndDialog()
      ENDIF
      RETURN .T.
   }
   LOCAL bCombo := {||
      oBrw2:aArray := SetBrw2( Self, oCombo:Value )
      oBrw2:Top()
      oBrw2:Refresh()
      RETURN .T.
   }
   LOCAL bPosChg2 := {||
      oEdit:SetText( oBrw2:aArray[oBrw2:nCurrent,1] )
      RETURN .T.
   }

   IF ::lToSave
      nPaneFHeight := 30
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
   oBrw1:bEnter := bEnter1
   oBrw1:bColor := ::aColors[CLR_STYLE]
   oBrw1:bColorSel := oBrw1:htbColor := ::aColors[CLR_BOARD]
   oBrw1:tColor := oBrw1:tColorSel := ::aColors[CLR_BTN2]
   oBrw1:lDispHead := oBrw1:lDispSep := .F.

   oBrw1:AddColumn( HColumn():New( "",{|v,o|o:aArray[o:nCurrent]},"C",32,0 ) )

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
      ON CLICK bEnter2

   ACTIVATE DIALOG oDlg

   RETURN cRes

METHOD Open( aFilters, cCurrPath, aColors ) CLASS HFileSelect

   LOCAL o := ::New( aFilters, cCurrPath, aColors )

   RETURN o:Show()

METHOD Save( aFilters, cCurrPath, aColors ) CLASS HFileSelect

   LOCAL o := ::New( aFilters, cCurrPath, aColors )

   o:lToSave := .T.

   RETURN o:Show()

STATIC FUNCTION SetBrw1( o, cPath )

   LOCAL arr, arr2, i

   IF '\' $ cPath
      cPath := StrTran( cPath, '\', '/' )
   ENDIF
   IF Right( cPath,1 ) == '/'
      cPath := hb_strShrink( cPath,1 )
   ENDIF
   arr := hb_ATokens( cPath, '/' )
   IF Empty( arr[1] )
      arr[1] := '/'
   ENDIF

   arr2 := Array( Len( arr ) )
   FOR i := 1 TO Len( arr )
      arr2[Len(arr)-i+1] := arr[i]
   NEXT

   RETURN arr2

STATIC FUNCTION SetBrw2( o, nItem )

   LOCAL aDir , i, j, n1 := 0, af, lf

   aDir := Directory( o:cCurrPath+"*", "HSD" )
   af := hb_ATokens( o:aFilters[nItem,2], ';' )

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

STATIC FUNCTION FSize( n )

   RETURN Iif( n<=999999, PAdl(Ltrim(Str(n)),6), ;
      Iif( n<10238976,PAdl(Left(Ltrim(Str(Round(n/1024,1))),5)+"K",6), ;
      Iif( n<10484711424, PAdl(Left(Ltrim(Str(Round(n/1048576,1))),5)+"M",6), ;
      PAdl(Left(Ltrim(Str(Round(n/1073741824,1))),5)+"G",6) ) ) )