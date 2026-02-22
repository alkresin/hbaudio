/*
 */

FUNCTION Main( cFile )

   LOCAL pDev, nSec

   IF cFile == Nil
      cFile := "out.wav"
   ENDIF

   IF Empty( pDev := ma_capture_init( cFile, 44100, 2 ) )
      ? "Capture init failed"
      RETURN Nil
   ENDIF

   ? "Begins after 3 seconds... "
   Inkey( 1 )
   ?? "2... "
   Inkey( 1 )
   ?? "1... "

   ? "Start!"
   ma_capture_start( pDev )
   nSec := Seconds()

   DO WHILE Seconds() - nSec < 30
      IF !Empty( Inkey() )
         EXIT
      ENDIF
      ma_sleep( 100 )
   ENDDO

   ma_capture_stop( pDev )
   ? "End"
   ma_capture_uninit( pDev )

   RETURN Nil