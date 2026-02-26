/*
 * sample_3.prg - A sample of playing an audio file
 *   using the Low Level API
 *
 * HbAudio - Harbour wrappers for miniaudio
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

STATIC pDevice := Nil

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

   ? "Start!"
   IF Empty( pDevice := ma_Device_Init( cFile ) )
      ? "ma_Device_Init() failed"
      RETURN Nil
   ENDIF

   nRate := ma_decoder_get_sample_rate( pDevice )
   nFrames := ma_decoder_get_length_in_pcm_frames( pDevice )

   ? nFrames/nRate, "seconds"
   ? "Playing... "
   ma_device_start( pDevice )

   DO WHILE !lExit
      DO WHILE ma_device_is_playing( pDevice )
         ma_sleep( 100 )
         IF ( nKey := Inkey() ) == K_ESC
            ma_device_stop( pDevice )
            lExit := .T.
            EXIT
         ELSEIF nKey == K_SPACE
            ma_device_stop( pDevice )
            lStopped := .T.
         ENDIF
         PrintProgress( ma_decoder_get_cursor_in_pcm_frames( pDevice ) * 100 / nFrames )
      ENDDO
      IF lStopped
         DO WHILE .T.
            ma_sleep( 100 )
            IF ( nKey := Inkey() ) == K_ESC
               lExit := .T.
               EXIT
            ELSEIF nKey == K_SPACE
               ma_device_start( pDevice )
               lStopped := .F.
               EXIT
            ENDIF
         ENDDO
      ELSE
         EXIT
      ENDIF
   ENDDO
   ma_device_uninit( pDevice )

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

   ? "Press any key to exit"
   Inkey(0)