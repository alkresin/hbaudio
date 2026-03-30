/*
 * HbAudio - Harbour wrappers for miniaudio
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define MINIAUDIO_IMPLEMENTATION
#define MA_NO_CUSTOM
//#define MA_SOUND_FLAG_NO_PITCH
//#define MA_SOUND_FLAG_NO_SPATIALIZATION
#include "miniaudio.h"

#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

typedef struct
{
   void          *pCoder;
   short int    bPlaying;
} udata;

void c_writelog( const char * sFile, const char * sTraceMsg, ... ) {
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

float sample_to_float( const void* pSample, ma_format format ) {

    float result;

    switch (format) {
        case ma_format_f32: {
            result = *((float*)pSample);
            break;
        }
        case ma_format_s16: {
            result = ( *((int16_t*)pSample) ) / 32768.0f;
            break;
        }
        case ma_format_s24: {
            int32_t val = *((int32_t*)pSample);
            val <<= 8;
            result = val / 2147483648.0f;
            break;
        }
        case ma_format_s32: {
            result = ( *((int32_t*)pSample) ) / 2147483648.0f;
            break;
        }
        case ma_format_u8: {
            result = ( *((uint8_t*)pSample) - 128 ) / 128.0f;
            break;
        }
        default:
            result = 0.0f;
    }

    return (result > 1.0f)? 1.0f : ( (result < -1.0f)? -1.0f : result );
}

void float_to_sample( void* pSample, ma_format format, float value ) {

    if (value > 1.0f) value = 1.0f;
    if (value < -1.0f) value = -1.0f;

    switch (format) {
        case ma_format_f32: {
            *((float*)pSample) = value;
            break;
        }
        case ma_format_s16: {
            int16_t int_val;
            if (value >= 0) {
                int_val = (int16_t)(value * 32767.0f + 0.5f);
            } else {
                int_val = (int16_t)(value * 32768.0f - 0.5f);
            }
            *((int16_t*)pSample) = int_val;
            break;
        }
        case ma_format_s24: {
            int32_t int_val;
            if (value >= 0) {
                int_val = (int32_t)(value * 8388607.0f + 0.5f);  // 2^23 - 1
            } else {
                int_val = (int32_t)(value * 8388608.0f - 0.5f);  // 2^23
            }
            int_val <<= 8;
            *((int32_t*)pSample) = int_val;
            break;
        }
        case ma_format_s32: {
            int32_t int_val;
            if (value >= 0) {
                int_val = (int32_t)(value * 2147483647.0 + 0.5);  // 2^31 - 1
            } else {
                int_val = (int32_t)(value * 2147483648.0 - 0.5);  // 2^31
            }
            *((int32_t*)pSample) = int_val;
            break;
        }
        case ma_format_u8: {
            uint8_t uint_val;
            if (value >= 0) {
                uint_val = (uint8_t)(value * 127.0f + 128.5f);
            } else {
                uint_val = (uint8_t)(value * 127.0f + 127.5f);
            }
            if (uint_val > 255) uint_val = 255;
            *((uint8_t*)pSample) = uint_val;
            break;
        }
        default:
            break;
    }
}

/* ma_engine_init() -> pEngine
 */
HB_FUNC( MA_ENGINE_INIT ) {

   ma_result result;
   ma_engine_config config;
   ma_engine * pEngine;

   pEngine = (ma_engine *) hb_xgrab( sizeof(ma_engine) );

   config = ma_engine_config_init();
   config.listenerCount = 1;

   result = ma_engine_init( &config, pEngine );
   if( result != MA_SUCCESS ) {
      hb_xfree( pEngine );
      hb_ret();
   } else {
      hb_retptr( (void*) pEngine );
   }

}

/* ma_engine_init( pEngine ) -> Nil
 */
HB_FUNC( MA_ENGINE_UNINIT ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );

   ma_engine_uninit( pEngine );
}

/* ma_engine_get_sample_rate( pEngine ) -> nRate
 */
HB_FUNC( MA_ENGINE_GET_SAMPLE_RATE ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );
   float fRate = ma_engine_get_sample_rate( pEngine );

   hb_retnd( fRate );
}

/* ma_engine_set_volume( pEngine, nVolume ) -> Nil
 */
HB_FUNC( MA_ENGINE_SET_VOLUME ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );

   ma_engine_set_volume( pEngine, hb_parnd( 2 ) );
}

/* ma_engine_get_volume( pEngine ) -> nVolume
 */
HB_FUNC( MA_ENGINE_GET_VOLUME ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );

   hb_retnd( ma_engine_get_volume( pEngine ) );
}

/* ma_Sound_Init( pEngine, cFile, lToDecodeOnly ) -> pSound
 */
HB_FUNC( MA_SOUND_INIT ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );
   ma_result result;
   ma_sound * pSound;
   ma_sound_config soundConfig;

   pSound = (ma_sound *) hb_xgrab( sizeof(ma_sound) );

   soundConfig = ma_sound_config_init();
   soundConfig.pFilePath = hb_parc(2);
   soundConfig.flags = ( (HB_ISLOG(3) && hb_parl(3))? MA_SOUND_FLAG_DECODE : MA_SOUND_FLAG_STREAM ) +
      MA_SOUND_FLAG_NO_SPATIALIZATION;

   result = ma_sound_init_ex( pEngine, &soundConfig, pSound );
   if(result != MA_SUCCESS) {
      hb_xfree( pSound );
      hb_ret();
   } else {
      hb_retptr( (void*) pSound );
   }

}

/* ma_sound_uninit( pSound ) -> Nil
 */
HB_FUNC( MA_SOUND_UNINIT ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   ma_sound_uninit( pSound );
   hb_xfree( pSound );
}

/* ma_sound_start( pSound ) -> nResult
 */
HB_FUNC( MA_SOUND_START ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   hb_retni( ma_sound_start( pSound ) );
}

/* ma_sound_is_playing( pSound ) -> lPlaying
 */
HB_FUNC( MA_SOUND_IS_PLAYING ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   hb_retl( ma_sound_is_playing( pSound ) );
}

/* ma_sound_stop( pSound ) -> nResult
 */
HB_FUNC( MA_SOUND_STOP ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   hb_retni( ma_sound_stop( pSound ) );
}

/* ma_sound_get_data_source( pSound ) -> pDataSource
 */
HB_FUNC( MA_SOUND_GET_DATA_SOURCE ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   hb_retptr( ma_sound_get_data_source( pSound ) );
}

/* ma_sound_get_channels( pSound ) -> nChannels
 */
HB_FUNC( MA_SOUND_GET_CHANNELS ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint32 channels;

   ma_data_source_get_data_format( ma_sound_get_data_source(pSound),
      NULL, &channels, NULL, NULL, 0 );
   hb_retni( channels );
}

/* ma_sound_set_volume( pSound, nVolume ) -> Nil
 */
HB_FUNC( MA_SOUND_SET_VOLUME ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   ma_sound_set_volume( pSound, hb_parnd( 2 ) );
}

/* ma_sound_get_volume( pSound ) -> nVolume
 */
HB_FUNC( MA_SOUND_GET_VOLUME ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   hb_retnd( ma_sound_get_volume( pSound ) );
}

/* ma_sound_get_length_in_pcm_frames( pSound ) -> nFramesAll
 */
HB_FUNC( MA_SOUND_GET_LENGTH_IN_PCM_FRAMES ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint64 lengthInFrames;

   ma_data_source_get_length_in_pcm_frames( ma_sound_get_data_source(pSound), &lengthInFrames );
   hb_retnl( (long) lengthInFrames );
}

/* ma_sound_get_cursor_in_pcm_frames( pSound ) -> nFrameCurrent
 */
HB_FUNC( MA_SOUND_GET_CURSOR_IN_PCM_FRAMES ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint64 cursor;

   ma_data_source_get_cursor_in_pcm_frames( ma_sound_get_data_source(pSound), &cursor );
   hb_retnl( (long) cursor );
}

/* ma_sound_seek_to_pcm_frame( pSound, nFrame ) -> Nil
 */
HB_FUNC( MA_SOUND_SEEK_TO_PCM_FRAME ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   ma_data_source_seek_to_pcm_frame( ma_sound_get_data_source(pSound), hb_parnl( 2 ) );
}

/* ma_sound_read_pcm_frames( pSound, pArrOut, nFrameStart, nFrameCount )
 */
HB_FUNC( MA_SOUND_READ_PCM_FRAMES ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint64 framesToRead = hb_parnl( 4 ), i;
   ma_uint32 channels = 1;
   ma_uint64 nRead = 0;
   char *pBuffer, *pSample;
   PHB_ITEM pArr, pSubArr;
   HB_TYPE type;
   int bArr;
   long int lArrLen;

   pArr = hb_param( 2, HB_IT_ARRAY );
   type = hb_arrayGetType( pArr, 1 );
   bArr = ( type & HB_IT_ARRAY );
   lArrLen = hb_arrayLen( pArr );

   ma_data_source* pDataSource = ma_sound_get_data_source( pSound );
   if( !HB_ISNIL(3) )
      ma_data_source_seek_to_pcm_frame( pDataSource, hb_parnl( 3 ) );

   ma_format format;
   ma_uint32 sampleRate;

   ma_data_source_get_data_format( pDataSource, &format, &channels, &sampleRate, NULL, 0 );

   ma_uint32 bytesPerFrame = ma_get_bytes_per_frame( format, channels );
   ma_uint32 bytesPerChannel = bytesPerFrame / channels;
   size_t bufferSize = (size_t)( framesToRead * bytesPerFrame );
   pBuffer = malloc( bufferSize );

   ma_data_source_read_pcm_frames( pDataSource, (void*)pBuffer, framesToRead, &nRead );
   //c_writelog( NULL, "read_frames-1 %lu %d %d\n", hb_parnl(3), result, nRead );

   if( nRead ) {
      pSample = pBuffer;
      for( i = 0; i < nRead && i+1 <= lArrLen; i ++, pSample += bytesPerFrame )
      {
         if( bArr )
         {
            pSubArr = hb_arrayGetItemPtr( pArr, i+1 );
            hb_arraySetND( pSubArr, 1, (double) sample_to_float( (float*)pSample, format ) );
            hb_arraySetND( pSubArr, 2, (double) sample_to_float(
               (float*)(pSample+bytesPerChannel), format ) );
         }
         else
            hb_arraySetND( pArr, i+1, (double) sample_to_float( (float*)pSample, format ) );
      }
   }

   free( pBuffer );
   hb_retnl( nRead );

}

/*
 * ma_GetRange( pSound, @ymax, @ymin ) -> nResult
 */
HB_FUNC( MA_GETRANGE ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_data_source* pDataSource = ma_sound_get_data_source( pSound );

   ma_result result;
   ma_format format;
   ma_uint32 channels;
   ma_uint32 sampleRate;

   result = ma_data_source_get_data_format(pDataSource, &format, &channels, &sampleRate, NULL, 0);
   if (result != MA_SUCCESS) {
       hb_retni( result );
       return;
   }

   // Go to the beginning
   result = ma_data_source_seek_to_pcm_frame( pDataSource, 0 );
   if (result != MA_SUCCESS) {
       hb_retni( result );
       c_writelog( NULL, "getrange-1a %d\n", result );
       return;
   }

   // Set the buffer size (4096 frames)
   ma_uint64 framesPerBatch = 4096;
   ma_uint32 bytesPerFrame = ma_get_bytes_per_frame( format, channels );
   size_t bufferSize = (size_t)( framesPerBatch * bytesPerFrame );
   void* buffer = malloc(bufferSize);
   if (buffer == NULL) {
       hb_retni( MA_OUT_OF_MEMORY );
       return;
   }

   float globalMin = 1000.0f;
   float globalMax = -1000.0f;

   // Read and analyse the data
   ma_uint64 framesRead;
   do {
        ma_uint64 cursor;
        ma_data_source_get_cursor_in_pcm_frames( pDataSource, &cursor );
        result = ma_data_source_read_pcm_frames( pDataSource, buffer, framesPerBatch, &framesRead );
        //c_writelog( NULL, "gr-1 %lu %d %d %f %f \n", cursor, result, framesRead, globalMin, globalMax );
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
            // Analyse due to format
            switch (format) {
                case ma_format_f32: {
                    float* samples = (float*)buffer;
                    //c_writelog( NULL, "getrange-ma_format_f32\n" );
                    for (ma_uint64 i = 0; i < framesRead * channels; i++, samples++) {
                        float sample = *samples;
                        if (sample < globalMin) globalMin = sample;
                        if (sample > globalMax) globalMax = sample;
                        //c_writelog( NULL, "   %f\n", sample );
                    }
                    break;
                }

                case ma_format_s16: {
                    ma_int16* samples = (ma_int16*)buffer;
                    for (ma_uint64 i = 0; i < framesRead * channels; i++) {
                        float sample = samples[i] / 32768.0f;
                        if (sample < globalMin) globalMin = sample;
                        if (sample > globalMax) globalMax = sample;
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

                        if (sampleFloat < globalMin) globalMin = sampleFloat;
                        if (sampleFloat > globalMax) globalMax = sampleFloat;
                    }
                    break;
                }

                case ma_format_s32: {
                    ma_int32* samples = (ma_int32*)buffer;
                    for (ma_uint64 i = 0; i < framesRead * channels; i++) {
                        float sample = samples[i] / 2147483648.0f; // 2^31
                        if (sample < globalMin) globalMin = sample;
                        if (sample > globalMax) globalMax = sample;
                    }
                    break;
                }

                case ma_format_u8: {
                    ma_uint8* samples = (ma_uint8*)buffer;
                    for (ma_uint64 i = 0; i < framesRead * channels; i++) {
                        float sample = (samples[i] / 128.0f) - 1.0f;
                        if (sample < globalMin) globalMin = sample;
                        if (sample > globalMax) globalMax = sample;
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

   result = ma_data_source_seek_to_pcm_frame(pDataSource, 0);
   if (result != MA_SUCCESS) {
       hb_retni( result );
       c_writelog( NULL, "getrange-2a %d\n", result );
   }

   free(buffer);
   hb_retni( MA_SUCCESS );

}

/* ma_get_bytes_per_frame( nFormat, nChannels ) -> nBytes
 */
HB_FUNC( MA_GET_BYTES_PER_FRAME ) {

   hb_retni( ma_get_bytes_per_frame( hb_parni( 1 ), hb_parni( 2 ) ) );

}

/* ma_get_pcm_frame( cArr, nFormat, nChannels, nFrame[, @dSecond] ) -> dValue
 */
HB_FUNC( MA_GET_PCM_FRAME ) {

   char *pSample;
   ma_format format = (ma_format) hb_parni( 2 );
   ma_uint32 channels = (ma_uint32) hb_parni( 3 );
   ma_uint32 bytesPerFrame = ma_get_bytes_per_frame( format, channels );

   pSample = ((char *) hb_parptr( 1 )) + ( hb_parnl(4) - 1 ) * bytesPerFrame;

   if( hb_pcount() > 4 )
      hb_stornd( (double) sample_to_float( (float*)(pSample + bytesPerFrame/channels), format ), 5 );
   hb_retnd( (double) sample_to_float( (float*)pSample, format ) );
}

/* ma_set_pcm_frame( cArr, nFormat, nChannels, nFrame, dValue1[, dValue2] )
 */
HB_FUNC( MA_SET_PCM_FRAME ) {

   char *pSample;
   ma_format format = (ma_format) hb_parni( 2 );
   ma_uint32 channels = (ma_uint32) hb_parni( 3 );
   ma_uint32 bytesPerFrame = ma_get_bytes_per_frame( format, channels );

   pSample = (char *) hb_parptr( 1 ) + ( hb_parnl(4) - 1 ) * bytesPerFrame;

   float_to_sample( (void*) pSample, format, hb_parnd( 5 ) );
   if( hb_pcount() > 5 )
      float_to_sample( (void*) (pSample + bytesPerFrame/channels), format, hb_parnd( 6 ) );
}

/* ma_data_source_set_loop_point_in_pcm_frames( pDataSource, loopBegInFrames, loopEndInFrames ) -> nResult
 */
HB_FUNC( MA_DATA_SOURCE_SET_LOOP_POINT_IN_PCM_FRAMES ) {

   ma_data_source* pDataSource = (ma_data_source*) hb_parptr( 1 );

   hb_retni( ma_data_source_set_loop_point_in_pcm_frames( pDataSource,
      hb_parnl( 2 ), hb_parnl( 3 ) ) );
}

/* ma_encoder_init( cFile, nFormat, nChannels, nSampleRate ) -> pEncoder
 */
HB_FUNC( MA_ENCODER_INIT ) {

   ma_encoder_config encoderConfig;
   ma_encoder *pEncoder;
   ma_result result;

   encoderConfig = ma_encoder_config_init( ma_encoding_format_wav, (ma_format) hb_parni( 2 ),
      (ma_uint32) hb_parni( 3 ), (ma_uint32) hb_parni( 4 ) );
   pEncoder = (ma_encoder *) hb_xgrab( sizeof(ma_encoder) );
   result = ma_encoder_init_file( hb_parc(1), &encoderConfig, pEncoder );
   if (result != MA_SUCCESS) {
      hb_xfree( pEncoder );
      hb_ret();
      return;
   }
   hb_retptr( pEncoder );
}

/* ma_encoder_uninit( pEncoder ) -> Nil
 */
HB_FUNC( MA_ENCODER_UNINIT ) {

   ma_encoder * pEncoder = (ma_encoder*) hb_parptr( 1 );
   ma_encoder_uninit( pEncoder );
   hb_xfree( pEncoder );
}

/* ma_encoder_write_pcm_frames( pEncoder,cBuffer, nFrames ) -> nFramesSaved
 */
HB_FUNC( MA_ENCODER_WRITE_PCM_FRAMES ) {

   ma_encoder * pEncoder = (ma_encoder*) hb_parptr( 1 );
   ma_result result;
   ma_uint64 frames_encoded = 0;

   result = ma_encoder_write_pcm_frames( pEncoder, (void*)hb_parc(2), hb_parnl(3), &frames_encoded );
   if (result != MA_SUCCESS )
      hb_retnl( -1 );
   else
      hb_retnl( frames_encoded );

}

/* ma_device_get_decoder( pDevice ) -> pDecoder
 */
HB_FUNC( MA_DEVICE_GET_DECODER ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;

   hb_retptr( pUData->pCoder );
}

/* ma_decoder_init( cFile ) -> pDecoder
 */
HB_FUNC( MA_DECODER_INIT ) {

   ma_decoder * pDecoder = (ma_decoder *) hb_xgrab( sizeof(ma_decoder) );
   ma_result result;

   result = ma_decoder_init_file( hb_parc(1), NULL, pDecoder );
   if (result != MA_SUCCESS) {
      hb_xfree( pDecoder );
      hb_ret();
      return;
   }
   hb_retptr( pDecoder );

}

/* ma_decoder_uninit( pDecoder ) -> Nil
 */
HB_FUNC( MA_DECODER_UNINIT ) {

   ma_decoder * pDecoder = (ma_decoder*) hb_parptr( 1 );
   ma_decoder_uninit( pDecoder );
   hb_xfree( pDecoder );
}

/* ma_decoder_get_info( pDecoder ) -> { nFormat, nChannels, nSampleRate }
 */
HB_FUNC( MA_DECODER_GET_INFO ) {

   ma_decoder * pDecoder = (ma_decoder*) hb_parptr( 1 );
   ma_format format;
   ma_uint32 channels;
   ma_uint32 sampleRate;
   PHB_ITEM aInfo = hb_itemArrayNew( 3 );

   ma_decoder_get_data_format( pDecoder, &format, &channels, &sampleRate, NULL, 0 );

   hb_itemPutNL( hb_arrayGetItemPtr( aInfo, 1 ), format );
   hb_itemPutNL( hb_arrayGetItemPtr( aInfo, 2 ), channels );
   hb_itemPutNL( hb_arrayGetItemPtr( aInfo, 3 ), sampleRate );
   hb_itemRelease( hb_itemReturn( aInfo ) );

}

/* ma_decoder_get_sample_rate( pDecoder ) -> nSampleRate
 */
HB_FUNC( MA_DECODER_GET_SAMPLE_RATE ) {

   ma_decoder * pDecoder = (ma_decoder*) hb_parptr( 1 );
   ma_uint32 sampleRate;

   ma_decoder_get_data_format( pDecoder, NULL, NULL, &sampleRate, NULL, 0 );
   hb_retnl( sampleRate );
}

/* ma_decoder_get_channels( pDecoder ) -> nChannels
 */
HB_FUNC( MA_DECODER_GET_CHANNELS ) {

   ma_decoder * pDecoder = (ma_decoder*) hb_parptr( 1 );
   ma_uint32 channels;

   ma_decoder_get_data_format( pDecoder, NULL, &channels, NULL, NULL, 0 );
   hb_retnl( channels );
}

/* ma_decoder_get_length_in_pcm_frames( pDecoder ) -> nFramesAll
 */
HB_FUNC( MA_DECODER_GET_LENGTH_IN_PCM_FRAMES ) {

   ma_decoder * pDecoder = (ma_decoder*) hb_parptr( 1 );
   ma_uint64 length;

   ma_decoder_get_length_in_pcm_frames( pDecoder, &length );
   hb_retnl( length );
}

/* ma_decoder_get_cursor_in_pcm_frames( pDecoder ) -> nFrameCurrent
 */
HB_FUNC( MA_DECODER_GET_CURSOR_IN_PCM_FRAMES ) {

   ma_decoder * pDecoder = (ma_decoder*) hb_parptr( 1 );
   ma_uint64 cursor;

   ma_decoder_get_cursor_in_pcm_frames( pDecoder, &cursor );
   hb_retnl( (long) cursor );
}

/*  ma_decoder_read_pcm_frames( pDecoder, cInputBuff, nFramesIn ) -> nRead
 */
HB_FUNC( MA_DECODER_READ_PCM_FRAMES ) {

   ma_decoder * pDecoder = (ma_decoder*) hb_parptr( 1 );
   ma_result result;
   ma_uint64 frames_read = 0;

   result = ma_decoder_read_pcm_frames( pDecoder, (void*)hb_parc(2), hb_parnl(3), &frames_read );
   if (result != MA_SUCCESS && result != MA_AT_END)
      hb_retnl( -1 );
   else
      hb_retnl( frames_read );

}

void data_callback(ma_device* pDevice, void* pOutput, const void* pInput, ma_uint32 frameCount)
{
   udata *pUData = (udata*)pDevice->pUserData;
   if( pUData == NULL ) return;
   ma_decoder* pDecoder = (ma_decoder*)(pUData->pCoder);
   if( pDecoder == NULL ) return;

   ma_uint64 framesRead;
   ma_decoder_read_pcm_frames( pDecoder, pOutput, frameCount, &framesRead );
   if( frameCount > framesRead )
      pUData->bPlaying = 0;
/*
   if( pUData->pSym_onEvent ) {
      hb_vmPushDynSym( pUData->pSym_onEvent );
      hb_vmPushNil();
      hb_vmPushPointer( ( void * )pOutput );
      hb_vmPushLong( framesRead );
      hb_vmDo( 2 );
   }
*/
   (void)pInput;
}

/* ma_device_playback_init( cFile ) -> pDevice
 */
HB_FUNC( MA_DEVICE_PLAYBACK_INIT ) {

   ma_result result;
   ma_device_config deviceConfig;
   ma_device * pDevice;
   ma_decoder * pDecoder;
   udata   * pUData;

   pDecoder = (ma_decoder *) hb_xgrab( sizeof(ma_decoder) );
   result = ma_decoder_init_file( hb_parc(1), NULL, pDecoder );
   if (result != MA_SUCCESS) {
      hb_xfree( pDecoder );
      hb_ret();
      return;
   }

   pUData = (udata *) hb_xgrab( sizeof(udata) );
   memset( pUData, 0, sizeof(udata) );
   pUData->pCoder = (void*)pDecoder;

   deviceConfig = ma_device_config_init( ma_device_type_playback );
   deviceConfig.playback.format   = pDecoder->outputFormat;
   deviceConfig.playback.channels = pDecoder->outputChannels;
   deviceConfig.sampleRate        = pDecoder->outputSampleRate;
   deviceConfig.dataCallback      = data_callback;
   deviceConfig.pUserData         = pUData;

   pDevice = (ma_device *) hb_xgrab( sizeof(ma_device) );
   if (ma_device_init( NULL, &deviceConfig, pDevice ) != MA_SUCCESS) {
      hb_xfree( pDevice );
      ma_decoder_uninit( pDecoder );
      hb_xfree( pDecoder );
      hb_xfree( pUData );
      hb_ret();
      return;
   }
/*
   if( HB_ISCHAR(2) ) {
      pUData->pSym_onEvent = hb_dynsymFindName( hb_parc(2) );
      if( !hb_dynsymIsFunction( pUData->pSym_onEvent ) )
         pUData->pSym_onEvent = NULL;
   }
*/
   hb_retptr( (void*) pDevice );
}

/* ma_device_playback_uninit( pDevice ) -> Nil
 */
HB_FUNC( MA_DEVICE_PLAYBACK_UNINIT ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;

   ma_decoder_uninit( (ma_decoder *)(pUData->pCoder) );
   ma_device_uninit( pDevice );
   hb_xfree( (ma_decoder *)(pUData->pCoder) );
   hb_xfree( pUData );
   hb_xfree( pDevice );
}

/* ma_device_start( pDevice ) -> nResult
 */
HB_FUNC( MA_DEVICE_START ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );

   ((udata*)pDevice->pUserData)->bPlaying = 1;
   hb_retni( ma_device_start( pDevice ) );
}

/* ma_device_stop( pDevice ) -> nResult
 */
HB_FUNC( MA_DEVICE_STOP ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );

   ((udata*)pDevice->pUserData)->bPlaying = 0;
   hb_retni( ma_device_stop( pDevice ) );
}

/* ma_device_is_playing( pDevice ) -> lPlaying
 */
HB_FUNC( MA_DEVICE_IS_PLAYING ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   hb_retl( ((udata*)pDevice->pUserData)->bPlaying );
}

/* ma_device_get_volume( pDevice ) -> nVolume
 */
HB_FUNC( MA_DEVICE_GET_VOLUME ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   float volume;

   ma_device_get_master_volume( pDevice, &volume );
   hb_retnd( volume );
}

/* ma_device_set_volume( pDevice, nVolume ) -> Nil
 */
HB_FUNC( MA_DEVICE_SET_VOLUME ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   ma_device_set_master_volume( pDevice, hb_parnd(2) );
}

// Callback function, called when receiving data from the microphone
void data_capture_callback( ma_device* pDevice, void* pOutput, const void* pInput, ma_uint32 frameCount )
{
   udata *pUData = (udata*)pDevice->pUserData;
   if( pUData == NULL ) return;
   ma_encoder* pEncoder = (ma_encoder*)(pUData->pCoder);
   if( pEncoder == NULL ) return;
/*
   if( pUData->pSym_onEvent ) {
      hb_vmPushDynSym( pUData->pSym_onEvent );
      hb_vmPushNil();
      hb_vmPushPointer( ( void * )pInput );
      hb_vmPushLong( frameCount );
      hb_vmDo( 2 );
      //return hb_parni( -1 );
   }
*/
    // Write the received data to the encoder (which saves it to a file)
    ma_encoder_write_pcm_frames( pEncoder, pInput, frameCount, NULL );

    (void)pOutput;
}

/* ma_device_capture_init( cFile, nSampleRate, nChannels ) -> pDevice
 */
HB_FUNC( MA_DEVICE_CAPTURE_INIT ) {

   const char* filename = HB_ISCHAR(1)? hb_parc( 1 ) : "out.wav";
   ma_uint32 sampleRate = HB_ISNUM(2)? hb_parni( 2 ) : 44100;
   ma_uint32 channels = HB_ISNUM(3)? hb_parni( 3 ) : 1;
   ma_result result;
   ma_device_config deviceConfig;
   ma_device * pDevice;
   ma_encoder_config encoderConfig;
   ma_encoder *pEncoder;
   udata   * pUData;

   // Configuring the encoder to save to a WAV file
   encoderConfig = ma_encoder_config_init( ma_encoding_format_wav, ma_format_s16, channels, sampleRate );
   pEncoder = (ma_encoder *) hb_xgrab( sizeof(ma_encoder) );
   result = ma_encoder_init_file(filename, &encoderConfig, pEncoder);
   if (result != MA_SUCCESS) {
      hb_xfree( pEncoder );
      hb_ret();
      return;
   }

   pUData = (udata *) hb_xgrab( sizeof(udata) );
   memset( pUData, 0, sizeof(udata) );
   pUData->pCoder = (void*)pEncoder;

   // Configuring the capture device
   deviceConfig = ma_device_config_init( ma_device_type_capture );
   deviceConfig.capture.format   = ma_format_s16;
   deviceConfig.capture.channels = channels;
   deviceConfig.sampleRate       = sampleRate;
   deviceConfig.pUserData        = pUData;
   deviceConfig.dataCallback     = data_capture_callback;

   // Initializing the device
   pDevice = (ma_device *) hb_xgrab( sizeof(ma_device) );
   result = ma_device_init( NULL, &deviceConfig, pDevice );
   if (result != MA_SUCCESS) {
      hb_xfree( pDevice );
      ma_encoder_uninit( pEncoder );
      hb_xfree( pEncoder );
      hb_xfree( pUData );
      hb_ret();
      return;
   }
/*
   if( HB_ISCHAR(4) ) {
      pUData->pSym_onEvent = hb_dynsymFindName( hb_parc(4) );
      if( !hb_dynsymIsFunction( pUData->pSym_onEvent ) )
         pUData->pSym_onEvent = NULL;
   }
*/
   hb_retptr( (void*) pDevice );
}

/* ma_device_capture_uninit( pDevice ) -> Nil
 */
HB_FUNC( MA_DEVICE_CAPTURE_UNINIT ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;

   ma_encoder_uninit( (ma_encoder *)(pUData->pCoder) );
   ma_device_uninit( pDevice );
   hb_xfree( (ma_encoder *)(pUData->pCoder) );
   hb_xfree( pUData );
   hb_xfree( pDevice );

}

/* ma_resampler_init( nFormat, nChannels, nSampleRateIn, nSampleRateOut ) -> pResampler
 */
HB_FUNC( MA_RESAMPLER_INIT ) {

   ma_resampler *pResampler;
   ma_result result;

   ma_resampler_config resampler_config = ma_resampler_config_init(
      (ma_format) hb_parni( 1 ), (ma_uint32) hb_parni( 2 ), (ma_uint32) hb_parni( 3 ),
      (ma_uint32) hb_parni( 4 ), ma_resample_algorithm_linear );

   pResampler = (ma_resampler *) hb_xgrab( sizeof(ma_resampler) );
   result = ma_resampler_init( &resampler_config, NULL, pResampler );
   if (result != MA_SUCCESS) {
      hb_ret();
      return;
   }
   hb_retptr( pResampler );
}

/* ma_resampler_uninit( pResampler ) -> Nil
 */
HB_FUNC( MA_RESAMPLER_UNINIT ) {

   ma_resampler *pResampler = (ma_resampler *) hb_parptr( 1 );

   ma_resampler_uninit( pResampler, NULL );
   hb_xfree( pResampler );
}

/*  ma_resampler_process_pcm_frames( pResampler, cBuffIn, cBuffOut, nFramesIn, nFramesOut ) -> nFramesWrittin
 */
HB_FUNC( MA_RESAMPLER_PROCESS_PCM_FRAMES ) {

   ma_resampler *pResampler = (ma_resampler *) hb_parptr( 1 );
   ma_result result;
   ma_uint64 frames_read = hb_parnl( 4 );
   ma_uint64 frames_written = hb_parnl( 5 );

   result = ma_resampler_process_pcm_frames( pResampler, (void*) hb_parc(2),
      &frames_read, (void*) hb_parc(3), &frames_written );

   if (result != MA_SUCCESS)
      hb_retnl( -1 );
   else
      hb_retnl( frames_written );
}

/* ma_sleep( nMilliseconds ) -> Nil
 */
HB_FUNC( MA_SLEEP ) {

   ma_sleep( hb_parni(1) );
}