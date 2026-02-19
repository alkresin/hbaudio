/*
 * Harbour wrappers for miniaudio
 */

#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"

#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

void c_writelog( const char * sFile, const char * sTraceMsg, ... )
{
   FILE *hFile;

   if( sFile == NULL )
   {
      hFile = fopen( "ac.log", "a" );
   }
   else
   {
      hFile = fopen( sFile, "a" );
   }

   if( hFile )
   {
      va_list ap;

      va_start( ap, sTraceMsg );
      vfprintf( hFile, sTraceMsg, ap );
      va_end( ap );

      fclose( hFile );
   }
}

HB_FUNC( MA_ENGINE_INIT ) {

   ma_result result;
   ma_engine_config config;
   ma_engine * pEngine;

   pEngine = (ma_engine *) hb_xgrab( sizeof(ma_engine) );

   config = ma_engine_config_init();
   config.listenerCount = 1;

   result = ma_engine_init( &config, pEngine );
   if(result != MA_SUCCESS) {
      hb_ret();
   } else {
      hb_retptr( (void*) pEngine );
   }

}

HB_FUNC( MA_ENGINE_GET_SAMPLE_RATE ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );
   float fRate = ma_engine_get_sample_rate( pEngine );

   hb_retnd( fRate );
}

HB_FUNC( MA_ENGINE_UNINIT ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );

   ma_engine_uninit( pEngine );
}

HB_FUNC( MA_ENGINE_SET_VOLUME ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );

   ma_engine_set_volume( pEngine, hb_parnd( 2 ) );
}

HB_FUNC( MA_ENGINE_GET_VOLUME ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );

   hb_retnd( ma_engine_get_volume( pEngine ) );
}

HB_FUNC( MA_SOUND_INIT ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );
   ma_result result;
   ma_sound * pSound;
   ma_sound_config soundConfig;

   pSound = (ma_sound *) hb_xgrab( sizeof(ma_sound) );

   soundConfig = ma_sound_config_init();
   soundConfig.pFilePath = hb_parc(2);
   soundConfig.flags = MA_SOUND_FLAG_STREAM;

   result = ma_sound_init_ex( pEngine, &soundConfig, pSound );
   if(result != MA_SUCCESS) {
      hb_ret();
   } else {
      hb_retptr( (void*) pSound );
   }

}

HB_FUNC( MA_SOUND_UNINIT ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   ma_sound_uninit( pSound );
   hb_xfree( pSound );
}

HB_FUNC( MA_SOUND_START ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   ma_sound_start( pSound );
}

HB_FUNC( MA_SOUND_IS_PLAYING ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   hb_retl( ma_sound_is_playing( pSound ) );
}

HB_FUNC( MA_SOUND_STOP ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   ma_sound_stop( pSound );
}

HB_FUNC( MA_SOUND_GET_CHANNELS ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint32 channels;

   ma_sound_get_data_format( pSound, NULL, &channels, NULL, NULL, 0 );
   hb_retni( channels );
}

HB_FUNC( MA_SOUND_SET_VOLUME ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   ma_sound_set_volume( pSound, hb_parnd( 2 ) );
}

HB_FUNC( MA_SOUND_GET_VOLUME ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   hb_retnd( ma_sound_get_volume( pSound ) );
}

HB_FUNC( MA_SOUND_GET_LENGTH_IN_PCM_FRAMES ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint64 lengthInFrames;

   ma_sound_get_length_in_pcm_frames( pSound, &lengthInFrames );
   hb_retnl( (long) lengthInFrames );
}

HB_FUNC( MA_SOUND_GET_CURSOR_IN_PCM_FRAMES ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint64 cursor;

   ma_sound_get_cursor_in_pcm_frames( pSound, &cursor );
   hb_retnl( (long) cursor );
}

HB_FUNC( MA_SOUND_SEEK_TO_PCM_FRAME ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   ma_sound_seek_to_pcm_frame( pSound, hb_parnl( 2 ) );
}

/* ma_sound_read_pcm_frames( pSound, pArrOut, nFrameStart, nFrameCount )
 */
HB_FUNC( MA_SOUND_READ_PCM_FRAMES ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint64 framesToRead = hb_parnl( 4 ), i;
   ma_uint32 channels = 1, j;
   ma_uint64 nRead = 0;
   float* pBuffer, * pOut;
   PHB_ITEM pArr, pSubArr;
   HB_TYPE type;
   int bArr;

   pArr = hb_param( 2, HB_IT_ARRAY );
   type = hb_arrayGetType( pArr, 1 );
   bArr = ( type & HB_IT_ARRAY );
   //c_writelog( NULL, "read_frames-1\n" );
   ma_sound_get_data_format( pSound, NULL, &channels, NULL, NULL, 0 );
   pBuffer = malloc( framesToRead * channels * sizeof(float) );

   if( !HB_ISNIL(3) )
      ma_sound_seek_to_pcm_frame( pSound, hb_parnl( 3 ) );

   ma_data_source_read_pcm_frames( ma_sound_get_data_source(pSound),
      pBuffer, framesToRead, &nRead );

   if( nRead ) {
      //if( channels > 1 )
      //   nRead /= channels;
      pOut = pBuffer;
      for( i = 1; i <= nRead; i += 1 )
      {
         if( bArr )
         {
            pSubArr = hb_arrayGetItemPtr( pArr, i+1 );
            for( j = 1; j <= channels; j++, pOut++ )
               hb_arraySetND( pSubArr, j, (double) *pOut );
         }
         else
         {
            hb_arraySetND( pArr, i+1, (double) *pOut );
            pOut ++;
         }
      }
   }

   free( pBuffer );
   hb_retnl( nRead );

}

/*
 * ma_GetRange( pSound, @ymax, @ymin )
 */
HB_FUNC( MA_GETRANGE ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_data_source* pDataSource = ma_sound_get_data_source( pSound );

   ma_result result;
   ma_format format;
   ma_uint32 channels;
   ma_uint32 sampleRate;

   // Получаем информацию о формате данных
   result = ma_data_source_get_data_format(pDataSource, &format, &channels, &sampleRate, NULL, 0);
   if (result != MA_SUCCESS) {
       hb_retni( result );
       return;
   }

   // Перемещаемся в начало
   result = ma_data_source_seek_to_pcm_frame(pDataSource, 0);
   if (result != MA_SUCCESS) {
       hb_retni( result );
       c_writelog( NULL, "getrange-3\n" );
       return;
   }

   // Определяем размер буфера (например, 4096 фреймов)
   ma_uint64 framesPerBatch = 4096;
   ma_uint32 bytesPerFrame = ma_get_bytes_per_frame(format, channels);
   size_t bufferSize = (size_t)(framesPerBatch * bytesPerFrame);
   void* buffer = malloc(bufferSize);
   if (buffer == NULL) {
       hb_retni( MA_OUT_OF_MEMORY );
       return;
   }

   // Инициализация переменных для поиска
   float globalMin = 0.0f;
   float globalMax = 0.0f;
   int firstSample = 1;

   // Читаем и анализируем данные порциями
   ma_uint64 framesRead;
   do {
        result = ma_data_source_read_pcm_frames(pDataSource, buffer, framesPerBatch, &framesRead);
        if (result == MA_BUSY ) {
           ma_sleep(1);
           continue;
        }
        if (result != MA_SUCCESS && result != MA_AT_END) {
            free(buffer);
            hb_retni( result );
            return;
        }

        if (framesRead > 0) {
            // Анализ в зависимости от формата
            switch (format) {
                case ma_format_f32: {
                    float* samples = (float*)buffer;
                    for (ma_uint64 i = 0; i < framesRead * channels; i++) {
                        float sample = samples[i];
                        if (firstSample) {
                            globalMin = sample;
                            globalMax = sample;
                            firstSample = 0;
                        } else {
                            if (sample < globalMin) globalMin = sample;
                            if (sample > globalMax) globalMax = sample;
                        }
                    }
                    break;
                }

                case ma_format_s16: {
                    ma_int16* samples = (ma_int16*)buffer;
                    for (ma_uint64 i = 0; i < framesRead * channels; i++) {
                        float sample = samples[i] / 32768.0f;
                        if (firstSample) {
                            globalMin = sample;
                            globalMax = sample;
                            firstSample = 0;
                        } else {
                            if (sample < globalMin) globalMin = sample;
                            if (sample > globalMax) globalMax = sample;
                        }
                    }
                    break;
                }

                case ma_format_s24: {
                    // Для 24-битных данных нужно особое внимание, так как они занимают 3 байта
                    ma_uint8* bytes = (ma_uint8*)buffer;
                    for (ma_uint64 i = 0; i < framesRead * channels; i++) {
                        // Собираем 24-битное значение (знаковое)
                        ma_int32 sample = (ma_int32)((bytes[0] << 8) | (bytes[1] << 16) | (bytes[2] << 24)) >> 8;
                        float sampleFloat = sample / 8388608.0f; // 2^23
                        bytes += 3;

                        if (firstSample) {
                            globalMin = sampleFloat;
                            globalMax = sampleFloat;
                            firstSample = 0;
                        } else {
                            if (sampleFloat < globalMin) globalMin = sampleFloat;
                            if (sampleFloat > globalMax) globalMax = sampleFloat;
                        }
                    }
                    break;
                }

                case ma_format_s32: {
                    ma_int32* samples = (ma_int32*)buffer;
                    for (ma_uint64 i = 0; i < framesRead * channels; i++) {
                        float sample = samples[i] / 2147483648.0f; // 2^31
                        if (firstSample) {
                            globalMin = sample;
                            globalMax = sample;
                            firstSample = 0;
                        } else {
                            if (sample < globalMin) globalMin = sample;
                            if (sample > globalMax) globalMax = sample;
                        }
                    }
                    break;
                }

                case ma_format_u8: {
                    ma_uint8* samples = (ma_uint8*)buffer;
                    for (ma_uint64 i = 0; i < framesRead * channels; i++) {
                        float sample = (samples[i] / 128.0f) - 1.0f;
                        if (firstSample) {
                            globalMin = sample;
                            globalMax = sample;
                            firstSample = 0;
                        } else {
                            if (sample < globalMin) globalMin = sample;
                            if (sample > globalMax) globalMax = sample;
                        }
                    }
                    break;
                }

                default:
                    free(buffer);
                    hb_retni( MA_INVALID_DATA );
                    return;
            }
        }

   } while (framesRead == framesPerBatch);

   hb_stornd( (double) globalMax, 2 );
   hb_stornd( (double) globalMin, 3 );

   free(buffer);
   hb_retni( MA_SUCCESS );

}

HB_FUNC( MA_SLEEP ) {

   ma_sleep( hb_parni(1) );
}