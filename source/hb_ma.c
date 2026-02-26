/*
 * HbAudio - Harbour wrappers for miniaudio
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"

#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

typedef struct
{
   void          *pCoder;
   PHB_DYNS pSym_onEvent;
   short int    bPlaying;
} udata;

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
   if( result != MA_SUCCESS ) {
      hb_xfree( pEngine );
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

/* ma_Sound_Init( pEngine, cFile, lToDecodeOnly )
 */
HB_FUNC( MA_SOUND_INIT ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );
   ma_result result;
   ma_sound * pSound;
   ma_sound_config soundConfig;

   pSound = (ma_sound *) hb_xgrab( sizeof(ma_sound) );

   soundConfig = ma_sound_config_init();
   soundConfig.pFilePath = hb_parc(2);
   soundConfig.flags = (HB_ISLOG(3) && hb_parl(3))? MA_SOUND_FLAG_DECODE : MA_SOUND_FLAG_STREAM;

   result = ma_sound_init_ex( pEngine, &soundConfig, pSound );
   if(result != MA_SUCCESS) {
      hb_xfree( pSound );
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

   hb_retni( ma_sound_start( pSound ) );
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

   ma_data_source_get_data_format( ma_sound_get_data_source(pSound),
      NULL, &channels, NULL, NULL, 0 );
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

   ma_data_source_get_length_in_pcm_frames( ma_sound_get_data_source(pSound), &lengthInFrames );
   hb_retnl( (long) lengthInFrames );
}

HB_FUNC( MA_SOUND_GET_CURSOR_IN_PCM_FRAMES ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint64 cursor;

   ma_data_source_get_cursor_in_pcm_frames( ma_sound_get_data_source(pSound), &cursor );
   hb_retnl( (long) cursor );
}

HB_FUNC( MA_SOUND_SEEK_TO_PCM_FRAME ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );

   ma_data_source_seek_to_pcm_frame( ma_sound_get_data_source(pSound), hb_parnl( 2 ) );
}

/* ma_sound_read_pcm_frames( pSound, pArrOut, nFrameStart, nFrameCount )
 */
HB_FUNC( MA_SOUND_READ_PCM_FRAMES ) {

   ma_sound * pSound = (ma_sound*) hb_parptr( 1 );
   ma_uint64 framesToRead = hb_parnl( 4 ), i;
   ma_uint32 channels = 1, j;
   ma_uint64 nRead = 0;
   //ma_result result;
   //float* pBuffer;
   float * pOut;
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

/*
   ma_sound_get_data_format( pSound, NULL, &channels, NULL, NULL, 0 );
   pBuffer = malloc( framesToRead * channels * sizeof(float) );
*/
   ma_format format;
   ma_uint32 sampleRate;

   ma_data_source_get_data_format( pDataSource, &format, &channels, &sampleRate, NULL, 0 );

   ma_uint32 bytesPerFrame = ma_get_bytes_per_frame( format, channels );
   size_t bufferSize = (size_t)( framesToRead * bytesPerFrame );
   void* pBuffer = malloc( bufferSize );

   ma_data_source_read_pcm_frames( pDataSource, pBuffer, framesToRead, &nRead );
   //c_writelog( NULL, "read_frames-1 %lu %d %d\n", hb_parnl(3), result, nRead );

   float globalMin = 1000.0f;
   float globalMax = -1000.0f;

   if( nRead ) {
      //if( channels > 1 )
      //   nRead /= channels;
      pOut = (float*)pBuffer;
      for( i = 1; i <= nRead && i+1 <= lArrLen; i += 1 )
      {
         if( bArr )
         {
            pSubArr = hb_arrayGetItemPtr( pArr, i+1 );
            for( j = 1; j <= channels; j++, pOut++ ) {
               if (*pOut < globalMin) globalMin = *pOut;
               if (*pOut > globalMax) globalMax = *pOut;
               hb_arraySetND( pSubArr, j, (double) *pOut );
            }
         }
         else
         {
            if (*pOut < globalMin) globalMin = *pOut;
            if (*pOut > globalMax) globalMax = *pOut;
            hb_arraySetND( pArr, i+1, (double) *pOut );
            pOut ++;
         }
      }
   }

   //c_writelog( NULL, "read_frames-1 %lu %d %d %f %f\n", hb_parnl(3), result, nRead, globalMin, globalMax );

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

HB_FUNC( MA_DECODER_GET_SAMPLE_RATE ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;
   ma_uint32 sampleRate;

   ma_decoder_get_data_format( (ma_decoder *)(pUData->pCoder),
      NULL, NULL, &sampleRate, NULL, 0 );
   hb_retnl( sampleRate );
}

HB_FUNC( MA_DECODER_GET_CHANNELS ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;
   ma_uint32 channels;

   ma_decoder_get_data_format( (ma_decoder *)(pUData->pCoder),
      NULL, &channels, NULL, NULL, 0 );
   hb_retnl( channels );
}

HB_FUNC( MA_DECODER_GET_LENGTH_IN_PCM_FRAMES ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;
   ma_uint64 length;

   ma_decoder_get_length_in_pcm_frames( (ma_decoder *)(pUData->pCoder), &length );
   hb_retnl( length );
}

HB_FUNC( MA_DECODER_GET_CURSOR_IN_PCM_FRAMES ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;
   ma_uint64 cursor;

   ma_decoder_get_cursor_in_pcm_frames( (ma_decoder *)(pUData->pCoder), &cursor );
   hb_retnl( (long) cursor );
}

void data_callback(ma_device* pDevice, void* pOutput, const void* pInput, ma_uint32 frameCount)
{
   udata *pUData = (udata*)pDevice->pUserData;
   if( pUData == NULL ) return;
   ma_decoder* pDecoder = (ma_decoder*)(pUData->pCoder);
   if( pDecoder == NULL ) return;

   if( pUData->pSym_onEvent ) {
      hb_vmPushDynSym( pUData->pSym_onEvent );
      hb_vmPushNil();
      hb_vmPushPointer( ( void * )pOutput );
      hb_vmDo( 1 );
   }

   ma_uint64 framesRead;
   ma_decoder_read_pcm_frames( pDecoder, pOutput, frameCount, &framesRead );
   if( frameCount > framesRead )
      pUData->bPlaying = 0;

   (void)pInput;
}

/* ma_device_init( cFile, cCallBackName )
 */
HB_FUNC( MA_DEVICE_INIT ) {

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

   if( HB_ISCHAR(2) ) {
      pUData->pSym_onEvent = hb_dynsymFindName( hb_parc(2) );
      if( !hb_dynsymIsFunction( pUData->pSym_onEvent ) )
         pUData->pSym_onEvent = NULL;
   }

   hb_retptr( (void*) pDevice );
}

HB_FUNC( MA_DEVICE_UNINIT ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;

   ma_decoder_uninit( (ma_decoder *)(pUData->pCoder) );
   ma_device_uninit( pDevice );
   hb_xfree( (ma_decoder *)(pUData->pCoder) );
   hb_xfree( pUData );
   hb_xfree( pDevice );
}

HB_FUNC( MA_DEVICE_START ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );

   ((udata*)pDevice->pUserData)->bPlaying = 1;
   hb_retni( ma_device_start( pDevice ) );
}

HB_FUNC( MA_DEVICE_STOP ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );

   ((udata*)pDevice->pUserData)->bPlaying = 0;
   hb_retni( ma_device_stop( pDevice ) );
}

HB_FUNC( MA_DEVICE_IS_PLAYING ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   hb_retl( ((udata*)pDevice->pUserData)->bPlaying );
}

HB_FUNC( MA_DEVICE_GET_VOLUME ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   float volume;

   ma_device_get_master_volume( pDevice, &volume );
   hb_retnd( volume );
}

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

   if( pUData->pSym_onEvent ) {
      hb_vmPushDynSym( pUData->pSym_onEvent );
      hb_vmPushNil();
      hb_vmPushPointer( ( void * )pInput );
      hb_vmDo( 1 );
      //return hb_parni( -1 );
   }
    // Write the received data to the encoder (which saves it to a file)
    ma_encoder_write_pcm_frames( pEncoder, pInput, frameCount, NULL );

    (void)pOutput;
}

/* ma_device_capture_init( cFile, nSampleRate, nChannels, cCallBackName )
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

   if( HB_ISCHAR(4) ) {
      pUData->pSym_onEvent = hb_dynsymFindName( hb_parc(4) );
      if( !hb_dynsymIsFunction( pUData->pSym_onEvent ) )
         pUData->pSym_onEvent = NULL;
   }

   hb_retptr( (void*) pDevice );
}

HB_FUNC( MA_DEVICE_CAPTURE_UNINIT ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;

   ma_encoder_uninit( (ma_encoder *)(pUData->pCoder) );
   ma_device_uninit( pDevice );
   hb_xfree( (ma_encoder *)(pUData->pCoder) );
   hb_xfree( pUData );
   hb_xfree( pDevice );

}

HB_FUNC( MA_SLEEP ) {

   ma_sleep( hb_parni(1) );
}