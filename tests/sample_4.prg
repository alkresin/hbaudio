/*
 * sample_4.prg - A sample of resampling an audio file
 *   using the Low Level API
 *
 * HbAudio - Harbour wrappers for miniaudio
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define FRAMES_PER_BATCH    8192

FUNCTION Main( cFile, nSampleRate )

   LOCAL pDecoder, pEncoder, pResampler
   LOCAL cFileOut := "out.wav", aInfo
   LOCAL cBuffIn, cBuffOut, nLenIn, nLenOut, nFramesOut
   LOCAL nRead, nReadAll := 0, nWritten, nWrAll := 0, nSaved, nSavedAll := 0

   pDecoder := ma_decoder_init( cFile )
   IF Empty( pDecoder )
      ? "Decoder init failed"
      RETURN Nil
   ENDIF

   aInfo := ma_decoder_get_info( pDecoder )

   IF nSampleRate == Nil
      nSampleRate := 16000
   ENDIF

   pEncoder := ma_encoder_init( cFileOut, aInfo[1], aInfo[2], nSampleRate )
   IF Empty( pEncoder )
      ? "Encoder init failed"
      ma_decoder_uninit( pDecoder )
      RETURN Nil
   ENDIF

   pResampler :=  ma_resampler_init( aInfo[1], aInfo[2], aInfo[3], nSampleRate )
   IF Empty( pResampler )
      ? "Resampler init failed"
      ma_decoder_uninit( pDecoder )
      ma_encoder_uninit( pEncoder )
      RETURN Nil
   ENDIF

   ? "Start"
   nLenIn := FRAMES_PER_BATCH * ma_get_bytes_per_frame( aInfo[1], aInfo[2] )
   nFramesOut := Int( FRAMES_PER_BATCH * nSampleRate / aInfo[3] ) + 10
   nLenOut  := nFramesOut * ma_get_bytes_per_frame( aInfo[1], aInfo[2] )
   cBuffIn  := Space( nLenIn )
   cBuffOut := Space( nLenOut )

   DO WHILE .T.
      nRead :=  ma_decoder_read_pcm_frames( pDecoder, cBuffIn, FRAMES_PER_BATCH )
      IF nRead <= 0
         EXIT
      ENDIF
      nReadAll += nRead

      nWritten := ma_resampler_process_pcm_frames( pResampler, cBuffIn, cBuffOut, nRead, ;
         Int( nRead * nSampleRate / aInfo[3] ) )
      nWrAll += nWritten

      IF nWritten > 0
         nSaved :=  ma_encoder_write_pcm_frames( pEncoder, cBuffOut, nWritten )
         nSavedAll += nSaved
      ENDIF

   ENDDO

   ma_resampler_uninit( pResampler )
   ma_encoder_uninit( pEncoder )
   ma_decoder_uninit( pDecoder )

   ? nReadAll, "read", nWrAll, "written", nSavedAll
   ? "End"

   RETURN Nil