/*
 * HbLame - Harbour wrappers for Lame library
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "lame.h"
#include "miniaudio.h"

#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

typedef struct
{
   void          *pCoder;
   short int    bPlaying;
} udata;

// Структура для WAV заголовка
typedef struct {
   char chunkID[4];        // "RIFF"
   unsigned int chunkSize;  // размер файла - 8
   char format[4];          // "WAVE"

   char subchunk1ID[4];     // "fmt "
   unsigned int subchunk1Size; // размер fmt данных (16 для PCM)
   unsigned short audioFormat; // 1 для PCM
   unsigned short numChannels;
   unsigned int sampleRate;
   unsigned int byteRate;
   unsigned short blockAlign;
   unsigned short bitsPerSample;

   char subchunk2ID[4];     // "data"
   unsigned int subchunk2Size; // размер данных
} WAVHeader;

#define INBUFFER_SIZE        4096
#define MP3BUFFER_SIZE       4096
#define BUFFER_SIZE_IN_FRAMES 512

/* lame_wav2mp3( cFileIn, cFileOut[, nBitRate] )
 */
HB_FUNC( LAME_WAV2MP3 ) {

   FILE *input_file, *output_file;
   int iBitRate = HB_ISNUM(3)? hb_parni(3) : 32;

   input_file = fopen( hb_parc(1), "rb");
   if( !input_file ) {
      hb_retni( 1 );
      return;
   }
   output_file = fopen( hb_parc(2), "wb");
   if( !output_file ) {
      fclose(input_file);
      hb_retni( 2 );
      return;
   }

   // Чтение WAV заголовка
   WAVHeader header;
   fread( &header, sizeof(WAVHeader), 1, input_file );

   // Проверка корректности WAV файла
   if( memcmp(header.chunkID, "RIFF", 4) != 0 || memcmp(header.format, "WAVE", 4) != 0 ) {
      fclose( input_file );
      fclose( output_file );
      hb_retni( 3 );
      return;
   }

   // Проверка формата (поддерживаем только PCM 16-bit)
   if( header.audioFormat != 1 || header.bitsPerSample != 16 ) {
      fclose( input_file );
      fclose( output_file );
      hb_retni( 4 );
      return;
   }

   // Инициализация LAME
   lame_global_flags *lame = lame_init();
   if( !lame ) {
      fclose( input_file );
      fclose( output_file );
      hb_retni( 5 );
      return;
   }

   // Установка параметров из WAV заголовка
   lame_set_num_channels( lame, header.numChannels );
   lame_set_in_samplerate( lame, header.sampleRate );
   lame_set_brate( lame, iBitRate );
   lame_set_mode( lame, header.numChannels == 1 ? MONO : STEREO );
   lame_set_quality( lame, 5 );

   // Инициализация параметров кодека
   if (lame_init_params(lame) < 0) {
       lame_close(lame);
       fclose(input_file);
       fclose(output_file);
      hb_retni( 6 );
      return;
   }

   // Буферы для чтения/записи
   short int *input_buffer = malloc(INBUFFER_SIZE * header.numChannels * sizeof(short int));
   unsigned char *mp3_buffer = malloc(MP3BUFFER_SIZE);

   int samples_read;
   int mp3_bytes;

   // Чтение PCM данных и кодирование
   while (1) {
       samples_read = fread( input_buffer, sizeof(short int) * header.numChannels, INBUFFER_SIZE, input_file );

       if( samples_read == 0 ) break;
       if( header.numChannels == 1 )
          mp3_bytes = lame_encode_buffer(lame, input_buffer, NULL, samples_read,
             mp3_buffer, MP3BUFFER_SIZE);
       else
          mp3_bytes = lame_encode_buffer_interleaved(lame, input_buffer, samples_read,
             mp3_buffer, MP3BUFFER_SIZE);

       if( mp3_bytes > 0 )
          fwrite( mp3_buffer, 1, mp3_bytes, output_file );
   }

   // Финализация MP3 потока
   mp3_bytes = lame_encode_flush( lame, mp3_buffer, MP3BUFFER_SIZE );
   if( mp3_bytes > 0 ) {
      fwrite( mp3_buffer, 1, mp3_bytes, output_file );
   }

   free(input_buffer);
   free(mp3_buffer);
   lame_close(lame);

   fclose( input_file );
   fclose( output_file );
   hb_retni( 0 );
}

typedef struct {
    lame_global_flags* lame;
    FILE* output_file;
    unsigned char* mp3_buffer;
    int mp3_buffer_size;
} CaptureContext;

void capture_callback(ma_device* pDevice, void* pOutput, const void* pInput, ma_uint32 frameCount) {

   udata *pUData = (udata*)pDevice->pUserData;
   CaptureContext* pctx = (CaptureContext*) (pUData->pCoder);

   // Игнорируем pOutput, так как мы только захватываем, не воспроизводим
   (void)pOutput;

   // Конвертируем захваченные PCM данные в MP3
   int mp3_bytes = lame_encode_buffer_interleaved(
       pctx->lame,
       (short*)pInput,  // Предполагаем ma_format_s16
       frameCount,
       pctx->mp3_buffer,
       pctx->mp3_buffer_size
   );

   if(mp3_bytes > 0 ) {
      fwrite( pctx->mp3_buffer, 1, mp3_bytes, pctx->output_file );
   }
}

/* lame_device_capture_init( cFile, nSampleRate, nChannels ) -> pDevice
 */
HB_FUNC( LAME_DEVICE_CAPTURE_INIT ) {

   const char* filename = HB_ISCHAR(1)? hb_parc( 1 ) : "out.mp3";
   ma_uint32 sampleRate = HB_ISNUM(2)? hb_parni( 2 ) : 44100;
   ma_uint32 channels = HB_ISNUM(3)? hb_parni( 3 ) : 1;
   ma_result result;
   ma_device_config deviceConfig;
   ma_device * pDevice;
   udata   * pUData;

   // Инициализация LAME
   lame_global_flags* lame = lame_init();
   lame_set_num_channels( lame, channels );
   lame_set_in_samplerate( lame, sampleRate );
   lame_set_brate( lame, 64 );
   lame_set_mode(lame, (channels==1)? MONO :STEREO );
   lame_set_quality(lame, 5);

   if (lame_init_params(lame) < 0) {
      hb_ret();
      return;
   }

   // Открываем выходной файл
   FILE* output_file = fopen( filename, "wb" );
   if (!output_file) {
      lame_close( lame );
      hb_ret();
      return;
   }

   CaptureContext * pctx = hb_xgrab( sizeof(CaptureContext) );
   pctx->lame = lame;
   pctx->output_file = output_file;
   pctx->mp3_buffer_size = BUFFER_SIZE_IN_FRAMES * 4;
   pctx->mp3_buffer = malloc(pctx->mp3_buffer_size);

   pUData = (udata *) hb_xgrab( sizeof(udata) );
   memset( pUData, 0, sizeof(udata) );
   pUData->pCoder = (void*) pctx;

   // Configuring the capture device
   deviceConfig = ma_device_config_init( ma_device_type_capture );
   deviceConfig.capture.format   = ma_format_s16;
   deviceConfig.capture.channels = channels;
   deviceConfig.sampleRate       = sampleRate;
   deviceConfig.pUserData        = pUData;
   deviceConfig.dataCallback     = capture_callback;

   // Initializing the device
   pDevice = (ma_device *) hb_xgrab( sizeof(ma_device) );
   result = ma_device_init( NULL, &deviceConfig, pDevice );
   if (result != MA_SUCCESS) {
      hb_xfree( pDevice );
      hb_xfree( pctx );
      hb_xfree( pUData );
      lame_close( lame );
      hb_ret();
      return;
   }

   hb_retptr( (void*) pDevice );

}

/* lame_device_capture_uninit( pDevice ) -> Nil
 */
HB_FUNC( LAME_DEVICE_CAPTURE_UNINIT ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;
   CaptureContext* pctx = (CaptureContext*) (pUData->pCoder);

   // Финализация MP3 (добавляем завершающие данные)
   int mp3_bytes = lame_encode_flush( pctx->lame, pctx->mp3_buffer, pctx->mp3_buffer_size );
   if( mp3_bytes > 0 ) {
       fwrite( pctx->mp3_buffer, 1, mp3_bytes, pctx->output_file );
   }

   free( pctx->mp3_buffer );
   fclose( pctx->output_file );
   lame_close( pctx->lame );

   ma_device_uninit( pDevice );
   hb_xfree( (CaptureContext*)(pUData->pCoder) );
   hb_xfree( pUData );
   hb_xfree( pDevice );
}