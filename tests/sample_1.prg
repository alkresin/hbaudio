/*
 */

FUNCTION Main()

   LOCAL pEngine

   IF Empty( pEngine := ma_Engine_Init() )
      ? "ma_Engine_Init() failed"
      RETURN Nil
   ENDIF

   ? "Start"


   ma_Engine_UnInit( pEngine )

   ? "Press any key to exit"
   Inkey(0)

   RETURN Nil