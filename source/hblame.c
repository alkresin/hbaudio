/*
 * HbLame - Harbour wrappers for Lame library
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#include "lame.h"

#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

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

#define INBUFFER_SIZE 4096
#define MP3BUFFER_SIZE 4096

/* lame_wav2mp3( cFileIn, cFileOut )
 */
HB_FUNC( LAME_WAV2MP3 ) {

   FILE *input_file = fopen( hb_parc(1), "rb");
   if( !input_file ) {
      hb_retni( 1 );
      return;
   }

   FILE *output_file = fopen( hb_parc(2), "wb");
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
   lame_set_brate( lame, 32 ); // ваш битрейт
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