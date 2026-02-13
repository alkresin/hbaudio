/*
 */
STATIC pEngine := Nil, pSound := Nil

#define K_ESC    27
#define K_SPACE  32

FUNCTION Main( cFile )

   LOCAL nRate, nFrames, nKey, lStopped := .F., lExit := .F.

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
   nFrames := ma_sound_get_length_in_pcm_frames( pSound )

   ? nFrames/nRate, "seconds"
   ? "Playing... "
   ma_sound_start( pSound )

   DO WHILE !lExit
      DO WHILE ma_sound_is_playing( pSound )
         ma_sleep( 100 )
         IF ( nKey := Inkey() ) == K_ESC
            ma_sound_stop( pSound )
            lExit := .T.
            EXIT
         ELSEIF nKey == K_SPACE
            ma_sound_stop( pSound )
            lStopped := .T.
         ENDIF
         PrintProgress( ma_sound_get_cursor_in_pcm_frames( pSound ) * 100 / nFrames )
      ENDDO
      IF lStopped
         DO WHILE .T.
            ma_sleep( 100 )
            IF ( nKey := Inkey() ) == K_ESC
               lExit := .T.
               EXIT
            ELSEIF nKey == K_SPACE
               ma_sound_start( pSound )
               lStopped := .F.
               EXIT
            ENDIF
         ENDDO
      ELSE
         EXIT
      ENDIF
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