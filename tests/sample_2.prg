/*
 * sample_2.prg - A sample, which captures the sound from a microphone
 *    and saves it in an audio file
 *
 * HbAudio - Harbour wrappers for miniaudio
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

FUNCTION Main( cFile )

   LOCAL pDev, nSec

   IF cFile == Nil
      cFile := "out.wav"
   ENDIF

   IF Empty( pDev := ma_device_capture_init( cFile, 44100, 2 ) )
      ? "Capture init failed"
      RETURN Nil
   ENDIF

   ? "Begins after 3 seconds... "
   Inkey( 1 )
   ?? "2... "
   Inkey( 1 )
   ?? "1... "

   ? "Start!"
   ma_device_capture_start( pDev )
   nSec := Seconds()

   DO WHILE Seconds() - nSec < 30
      IF !Empty( Inkey() )
         EXIT
      ENDIF
      ma_sleep( 100 )
   ENDDO

   ma_device_capture_stop( pDev )
   ? "End"
   ma_device_capture_uninit( pDev )

   RETURN Nil