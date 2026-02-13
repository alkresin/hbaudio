/*
 */
STATIC pEngine := Nil, pSound := Nil

#define K_ESC    27

FUNCTION Main( cFile )

   LOCAL nRate, nFrames

   SET CURSOR OFF

   IF Empty( cFile )
      ? "File name absent"
      RETURN Nil
   ELSEIF !File( cFile )
      ? cFile, "isn't found"
      RETURN Nil
   ENDIF

   IF Empty( pEngine := ma_Engine_Init() )
      ? "ma_Engine_Init() failed"
      RETURN Nil
   ENDIF

   IF Empty( pSound := ma_Sound_Init( pEngine, cFile ) )
      ? "ma_Sound_Init() failed"
      RETURN Nil
   ENDIF

   nRate := ma_engine_get_sample_rate( pEngine )
   nFrames := ma_data_source_get_length_in_pcm_frames( pSound )

   ? nFrames/nRate, "seconds"
   ? "Playing... "
   ma_sound_start( pSound )

   DO WHILE ma_sound_is_playing( pSound )
      IF Inkey() == K_ESC
         ma_sound_stop( pSound )
         EXIT
      ENDIF
      PrintProgress( ma_data_source_get_cursor_in_pcm_frames( pSound ) * 100 / nFrames )
      ma_sleep( 100 )
   ENDDO

   ma_sound_uninit( pSound )

   RETURN Nil

STATIC FUNCTION PrintProgress( nCurr )

   LOCAL nRow := Row(), nCol := Col()
   STATIC nLast := -2

   IF nCurr - nLast > 1
      ?? Ltrim( Str( Int( nCurr ) ) ) + '%'
      nLast := nCurr
      Setpos( nRow, nCol )
   ENDIF

   RETURN Nil

EXIT PROCEDURE FExit

   IF !Empty( pEngine )
      ma_Engine_UnInit( pEngine )
   ENDIF
   ? "Press any key to exit"
   Inkey(0)

   RETURN