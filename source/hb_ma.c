/*
 * Harbour wrappers for miniaudio
 */

#define MINIAUDIO_IMPLEMENTATION
#include "miniaudio.h"

#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

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

HB_FUNC( MA_SOUND_INIT ) {

   ma_engine * pEngine = (ma_engine*) hb_parptr( 1 );
   ma_result result;
   ma_sound * pSound;
   ma_sound_config soundConfig;

   pSound = (ma_sound *) hb_xgrab( sizeof(ma_sound) );

   soundConfig = ma_sound_config_init();
   soundConfig.pFilePath = hb_parc(2);
   soundConfig.flags = MA_SOUND_FLAG_STREAM;  // Потоковое чтение

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

HB_FUNC( MA_SLEEP ) {

   ma_sleep( hb_parni(1) );
}