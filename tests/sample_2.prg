/*
 */

FUNCTION Main( cFile )

   LOCAL pDev

   IF cFile == Nil
      cFile := "out.wav"
   ENDIF

   IF Empty( pDev := ma_capture_init( cFile, 44100, 2 ) )
      ? "Capture init failed"
      RETURN Nil
   ENDIF

   ? "Start!"
   ma_capture_start( pDev )
   ma_sleep( 5000 )
   ma_capture_stop( pDev )
   ? "End"
   ma_capture_uninit( pDev )

   RETURN Nil