// Python.h in order to define off_t to 64 bits
#include <Python.h> 
#include <sys/types.h>
#include <stdio.h>
#include <assert.h>
#include "bzlib.h"


/*-------------------------------------------------------------*/
/*--- Library top-level functions.                          ---*/
/*---                                               bzlib.c ---*/
/*-------------------------------------------------------------*/

/*--
  This file is a part of bzip2 and/or libbzip2, a program and
  library for lossless, block-sorting data compression.

  Copyright (C) 1996-2005 Julian R Seward.  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions
  are met:

  1. Redistributions of source code must retain the above copyright
     notice, this list of conditions and the following disclaimer.

  2. The origin of this software must not be misrepresented; you must 
     not claim that you wrote the original software.  If you use this 
     software in a product, an acknowledgment in the product 
     documentation would be appreciated but is not required.

  3. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.

  4. The name of the author may not be used to endorse or promote 
     products derived from this software without specific prior written 
     permission.

  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
  GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Julian Seward, Cambridge, UK.
  jseward@bzip.org
  bzip2/libbzip2 version 1.0 of 21 March 2000

  This program is based on (at least) the work of:
     Mike Burrows
     David Wheeler
     Peter Fenwick
     Alistair Moffat
     Radford Neal
     Ian H. Witten
     Robert Sedgewick
     Jon L. Bentley

  For more information on these sources, see the manual.
--*/

/*--
   CHANGES
   ~~~~~~~
   0.9.0 -- original version.

   0.9.0a/b -- no changes in this file.

   0.9.0c
      * made zero-length BZ_FLUSH work correctly in bzCompress().
      * fixed bzWrite/bzRead to ignore zero-length requests.
      * fixed bzread to correctly handle read requests after EOF.
      * wrong parameter order in call to bzDecompressInit in
        bzBuffToBuffDecompress.  Fixed.
--*/

#include "bzlib_private.h"


/*---------------------------------------------------*/
/*--- Compression stuff                           ---*/
/*---------------------------------------------------*/


/*---------------------------------------------------*/
#ifndef BZ_NO_STDIO
void BZ2_bz__AssertH__fail ( int errcode )
{
   fprintf(stderr, 
      "\n\nbzip2/libbzip2: internal error number %d.\n"
      "This is a bug in bzip2/libbzip2, %s.\n"
      "Please report it to me at: jseward@bzip.org.  If this happened\n"
      "when you were using some program which uses libbzip2 as a\n"
      "component, you should also report this bug to the author(s)\n"
      "of that program.  Please make an effort to report this bug;\n"
      "timely and accurate bug reports eventually lead to higher\n"
      "quality software.  Thanks.  Julian Seward, 15 February 2005.\n\n",
      errcode,
      "hacked"
   );

   if (errcode == 1007) {
   fprintf(stderr,
      "\n*** A special note about internal error number 1007 ***\n"
      "\n"
      "Experience suggests that a common cause of i.e. 1007\n"
      "is unreliable memory or other hardware.  The 1007 assertion\n"
      "just happens to cross-check the results of huge numbers of\n"
      "memory reads/writes, and so acts (unintendedly) as a stress\n"
      "test of your memory system.\n"
      "\n"
      "I suggest the following: try compressing the file again,\n"
      "possibly monitoring progress in detail with the -vv flag.\n"
      "\n"
      "* If the error cannot be reproduced, and/or happens at different\n"
      "  points in compression, you may have a flaky memory system.\n"
      "  Try a memory-test program.  I have used Memtest86\n"
      "  (www.memtest86.com).  At the time of writing it is free (GPLd).\n"
      "  Memtest86 tests memory much more thorougly than your BIOSs\n"
      "  power-on test, and may find failures that the BIOS doesn't.\n"
      "\n"
      "* If the error can be repeatably reproduced, this is a bug in\n"
      "  bzip2, and I would very much like to hear about it.  Please\n"
      "  let me know, and, ideally, save a copy of the file causing the\n"
      "  problem -- without which I will be unable to investigate it.\n"
      "\n"
   );
   }

   exit(3);
}
#endif


/*---------------------------------------------------*/
static
int bz_config_ok ( void )
{
   if (sizeof(int)   != 4) return 0;
   if (sizeof(short) != 2) return 0;
   if (sizeof(char)  != 1) return 0;
   return 1;
}


/*---------------------------------------------------*/
static
void* default_bzalloc ( void* opaque, Int32 items, Int32 size )
{
   void* v = malloc ( items * size );
   return v;
}

static
void default_bzfree ( void* opaque, void* addr )
{
   if (addr != NULL) free ( addr );
}


/*---------------------------------------------------*/
static
void prepare_new_block ( EState* s )
{
   Int32 i;
   s->nblock = 0;
   s->numZ = 0;
   s->state_out_pos = 0;
   BZ_INITIALISE_CRC ( s->blockCRC );
   for (i = 0; i < 256; i++) s->inUse[i] = False;
   s->blockNo++;
}


/*---------------------------------------------------*/
static
void init_RL ( EState* s )
{
   s->state_in_ch  = 256;
   s->state_in_len = 0;
}


static
Bool isempty_RL ( EState* s )
{
   if (s->state_in_ch < 256 && s->state_in_len > 0)
      return False; else
      return True;
}


/*---------------------------------------------------*/
static
void add_pair_to_block ( EState* s )
{
   Int32 i;
   UChar ch = (UChar)(s->state_in_ch);
   for (i = 0; i < s->state_in_len; i++) {
      BZ_UPDATE_CRC( s->blockCRC, ch );
   }
   s->inUse[s->state_in_ch] = True;
   switch (s->state_in_len) {
      case 1:
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         break;
      case 2:
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         break;
      case 3:
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         break;
      default:
         s->inUse[s->state_in_len-4] = True;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = (UChar)ch; s->nblock++;
         s->block[s->nblock] = ((UChar)(s->state_in_len-4));
         s->nblock++;
         break;
   }
}


/*---------------------------------------------------*/
static
void flush_RL ( EState* s )
{
   if (s->state_in_ch < 256) add_pair_to_block ( s );
   init_RL ( s );
}


/*---------------------------------------------------*/
#define ADD_CHAR_TO_BLOCK(zs,zchh0)               \
{                                                 \
   UInt32 zchh = (UInt32)(zchh0);                 \
   /*-- fast track the common case --*/           \
   if (zchh != zs->state_in_ch &&                 \
       zs->state_in_len == 1) {                   \
      UChar ch = (UChar)(zs->state_in_ch);        \
      BZ_UPDATE_CRC( zs->blockCRC, ch );          \
      zs->inUse[zs->state_in_ch] = True;          \
      zs->block[zs->nblock] = (UChar)ch;          \
      zs->nblock++;                               \
      zs->state_in_ch = zchh;                     \
   }                                              \
   else                                           \
   /*-- general, uncommon cases --*/              \
   if (zchh != zs->state_in_ch ||                 \
      zs->state_in_len == 255) {                  \
      if (zs->state_in_ch < 256)                  \
         add_pair_to_block ( zs );                \
      zs->state_in_ch = zchh;                     \
      zs->state_in_len = 1;                       \
   } else {                                       \
      zs->state_in_len++;                         \
   }                                              \
}


/*---------------------------------------------------*/
static
Bool copy_input_until_stop ( EState* s )
{
   Bool progress_in = False;

   if (s->mode == BZ_M_RUNNING) {

      /*-- fast track the common case --*/
      while (True) {
         /*-- block full? --*/
         if (s->nblock >= s->nblockMAX) break;
         /*-- no input? --*/
         if (s->strm->avail_in == 0) break;
         progress_in = True;
         ADD_CHAR_TO_BLOCK ( s, (UInt32)(*((UChar*)(s->strm->next_in))) ); 
         s->strm->next_in++;
         s->strm->avail_in--;
         s->strm->total_in_lo32++;
         if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++;
      }

   } else {

      /*-- general, uncommon case --*/
      while (True) {
         /*-- block full? --*/
         if (s->nblock >= s->nblockMAX) break;
         /*-- no input? --*/
         if (s->strm->avail_in == 0) break;
         /*-- flush/finish end? --*/
         if (s->avail_in_expect == 0) break;
         progress_in = True;
         ADD_CHAR_TO_BLOCK ( s, (UInt32)(*((UChar*)(s->strm->next_in))) ); 
         s->strm->next_in++;
         s->strm->avail_in--;
         s->strm->total_in_lo32++;
         if (s->strm->total_in_lo32 == 0) s->strm->total_in_hi32++;
         s->avail_in_expect--;
      }
   }
   return progress_in;
}


/*---------------------------------------------------*/

/*---------------------------------------------------*/

/*---------------------------------------------------*/
/*--- Decompression stuff                         ---*/
/*---------------------------------------------------*/

/*---------------------------------------------------*/
int BZ_API(BZ2_bzDecompressInit) 
                     ( bz_stream* strm, 
                       int        verbosity,
                       int        small )
{
   DState* s;

   if (!bz_config_ok()) return BZ_CONFIG_ERROR;

   if (strm == NULL) return BZ_PARAM_ERROR;
   if (small != 0 && small != 1) return BZ_PARAM_ERROR;
   if (verbosity < 0 || verbosity > 4) return BZ_PARAM_ERROR;

   if (strm->bzalloc == NULL) strm->bzalloc = default_bzalloc;
   if (strm->bzfree == NULL) strm->bzfree = default_bzfree;

   s = BZALLOC( sizeof(DState) );
   if (s == NULL) return BZ_MEM_ERROR;
   s->strm                  = strm;
   strm->state              = s;
   s->state                 = BZ_X_MAGIC_1;
   s->bsLive                = 0;
   s->bsBuff                = 0;
   s->calculatedCombinedCRC = 0;
   strm->total_in_lo32      = 0;
   strm->total_in_hi32      = 0;
   strm->total_out_lo32     = 0;
   strm->total_out_hi32     = 0;
   s->smallDecompress       = (Bool)small;
   s->ll4                   = NULL;
   s->ll16                  = NULL;
   s->tt                    = NULL;
   s->currBlockNo           = 0;
   s->verbosity             = verbosity;

   return BZ_OK;
}


/*---------------------------------------------------*/
/* Return  True iff data corruption is discovered.
   Returns False if there is no problem.
*/
static
Bool unRLE_obuf_to_output_FAST ( DState* s )
{
   UChar k1;

   if (s->blockRandomised) {

      while (True) {
         /* try to finish existing run */
         while (True) {
            if (s->strm->avail_out == 0) return False;
            if (s->state_out_len == 0) break;
            *( (UChar*)(s->strm->next_out) ) = s->state_out_ch;
            BZ_UPDATE_CRC ( s->calculatedBlockCRC, s->state_out_ch );
            s->state_out_len--;
            s->strm->next_out++;
            s->strm->avail_out--;
            s->strm->total_out_lo32++;
            if (s->strm->total_out_lo32 == 0) s->strm->total_out_hi32++;
         }

         /* can a new run be started? */
         if (s->nblock_used == s->save_nblock+1) return False;
               
         /* Only caused by corrupt data stream? */
         if (s->nblock_used > s->save_nblock+1)
            return True;
   
         s->state_out_len = 1;
         s->state_out_ch = s->k0;
         BZ_GET_FAST(k1); BZ_RAND_UPD_MASK; 
         k1 ^= BZ_RAND_MASK; s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };
   
         s->state_out_len = 2;
         BZ_GET_FAST(k1); BZ_RAND_UPD_MASK; 
         k1 ^= BZ_RAND_MASK; s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };
   
         s->state_out_len = 3;
         BZ_GET_FAST(k1); BZ_RAND_UPD_MASK; 
         k1 ^= BZ_RAND_MASK; s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };
   
         BZ_GET_FAST(k1); BZ_RAND_UPD_MASK; 
         k1 ^= BZ_RAND_MASK; s->nblock_used++;
         s->state_out_len = ((Int32)k1) + 4;
         BZ_GET_FAST(s->k0); BZ_RAND_UPD_MASK; 
         s->k0 ^= BZ_RAND_MASK; s->nblock_used++;
      }

   } else {

      /* restore */
      UInt32        c_calculatedBlockCRC = s->calculatedBlockCRC;
      UChar         c_state_out_ch       = s->state_out_ch;
      Int32         c_state_out_len      = s->state_out_len;
      Int32         c_nblock_used        = s->nblock_used;
      Int32         c_k0                 = s->k0;
      UInt32*       c_tt                 = s->tt;
      UInt32        c_tPos               = s->tPos;
      char*         cs_next_out          = s->strm->next_out;
      unsigned int  cs_avail_out         = s->strm->avail_out;
      /* end restore */

      UInt32       avail_out_INIT = cs_avail_out;
      Int32        s_save_nblockPP = s->save_nblock+1;
      unsigned int total_out_lo32_old;

      while (True) {

         /* try to finish existing run */
         if (c_state_out_len > 0) {
            while (True) {
               if (cs_avail_out == 0) goto return_notr;
               if (c_state_out_len == 1) break;
               *( (UChar*)(cs_next_out) ) = c_state_out_ch;
               BZ_UPDATE_CRC ( c_calculatedBlockCRC, c_state_out_ch );
               c_state_out_len--;
               cs_next_out++;
               cs_avail_out--;
            }
            s_state_out_len_eq_one:
            {
               if (cs_avail_out == 0) { 
                  c_state_out_len = 1; goto return_notr;
               };
               *( (UChar*)(cs_next_out) ) = c_state_out_ch;
               BZ_UPDATE_CRC ( c_calculatedBlockCRC, c_state_out_ch );
               cs_next_out++;
               cs_avail_out--;
            }
         }   
         /* Only caused by corrupt data stream? */
         if (c_nblock_used > s_save_nblockPP)
            return True;

         /* can a new run be started? */
         if (c_nblock_used == s_save_nblockPP) {
            c_state_out_len = 0; goto return_notr;
         };   
         c_state_out_ch = c_k0;
         BZ_GET_FAST_C(k1); c_nblock_used++;
         if (k1 != c_k0) { 
            c_k0 = k1; goto s_state_out_len_eq_one; 
         };
         if (c_nblock_used == s_save_nblockPP) 
            goto s_state_out_len_eq_one;
   
         c_state_out_len = 2;
         BZ_GET_FAST_C(k1); c_nblock_used++;
         if (c_nblock_used == s_save_nblockPP) continue;
         if (k1 != c_k0) { c_k0 = k1; continue; };
   
         c_state_out_len = 3;
         BZ_GET_FAST_C(k1); c_nblock_used++;
         if (c_nblock_used == s_save_nblockPP) continue;
         if (k1 != c_k0) { c_k0 = k1; continue; };
   
         BZ_GET_FAST_C(k1); c_nblock_used++;
         c_state_out_len = ((Int32)k1) + 4;
         BZ_GET_FAST_C(c_k0); c_nblock_used++;
      }

      return_notr:
      total_out_lo32_old = s->strm->total_out_lo32;
      s->strm->total_out_lo32 += (avail_out_INIT - cs_avail_out);
      if (s->strm->total_out_lo32 < total_out_lo32_old)
         s->strm->total_out_hi32++;

      /* save */
      s->calculatedBlockCRC = c_calculatedBlockCRC;
      s->state_out_ch       = c_state_out_ch;
      s->state_out_len      = c_state_out_len;
      s->nblock_used        = c_nblock_used;
      s->k0                 = c_k0;
      s->tt                 = c_tt;
      s->tPos               = c_tPos;
      s->strm->next_out     = cs_next_out;
      s->strm->avail_out    = cs_avail_out;
      /* end save */
   }
   return False;
}



/*---------------------------------------------------*/
__inline__ Int32 BZ2_indexIntoF ( Int32 indx, Int32 *cftab )
{
   Int32 nb, na, mid;
   nb = 0;
   na = 256;
   do {
      mid = (nb + na) >> 1;
      if (indx >= cftab[mid]) nb = mid; else na = mid;
   }
   while (na - nb != 1);
   return nb;
}


/*---------------------------------------------------*/
/* Return  True iff data corruption is discovered.
   Returns False if there is no problem.
*/
static
Bool unRLE_obuf_to_output_SMALL ( DState* s )
{
   UChar k1;

   if (s->blockRandomised) {

      while (True) {
         /* try to finish existing run */
         while (True) {
            if (s->strm->avail_out == 0) return False;
            if (s->state_out_len == 0) break;
            *( (UChar*)(s->strm->next_out) ) = s->state_out_ch;
            BZ_UPDATE_CRC ( s->calculatedBlockCRC, s->state_out_ch );
            s->state_out_len--;
            s->strm->next_out++;
            s->strm->avail_out--;
            s->strm->total_out_lo32++;
            if (s->strm->total_out_lo32 == 0) s->strm->total_out_hi32++;
         }
   
         /* can a new run be started? */
         if (s->nblock_used == s->save_nblock+1) return False;

         /* Only caused by corrupt data stream? */
         if (s->nblock_used > s->save_nblock+1)
            return True;
   
         s->state_out_len = 1;
         s->state_out_ch = s->k0;
         BZ_GET_SMALL(k1); BZ_RAND_UPD_MASK; 
         k1 ^= BZ_RAND_MASK; s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };
   
         s->state_out_len = 2;
         BZ_GET_SMALL(k1); BZ_RAND_UPD_MASK; 
         k1 ^= BZ_RAND_MASK; s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };
   
         s->state_out_len = 3;
         BZ_GET_SMALL(k1); BZ_RAND_UPD_MASK; 
         k1 ^= BZ_RAND_MASK; s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };
   
         BZ_GET_SMALL(k1); BZ_RAND_UPD_MASK; 
         k1 ^= BZ_RAND_MASK; s->nblock_used++;
         s->state_out_len = ((Int32)k1) + 4;
         BZ_GET_SMALL(s->k0); BZ_RAND_UPD_MASK; 
         s->k0 ^= BZ_RAND_MASK; s->nblock_used++;
      }

   } else {

      while (True) {
         /* try to finish existing run */
         while (True) {
            if (s->strm->avail_out == 0) return False;
            if (s->state_out_len == 0) break;
            *( (UChar*)(s->strm->next_out) ) = s->state_out_ch;
            BZ_UPDATE_CRC ( s->calculatedBlockCRC, s->state_out_ch );
            s->state_out_len--;
            s->strm->next_out++;
            s->strm->avail_out--;
            s->strm->total_out_lo32++;
            if (s->strm->total_out_lo32 == 0) s->strm->total_out_hi32++;
         }
   
         /* can a new run be started? */
         if (s->nblock_used == s->save_nblock+1) return False;

         /* Only caused by corrupt data stream? */
         if (s->nblock_used > s->save_nblock+1)
            return True;
   
         s->state_out_len = 1;
         s->state_out_ch = s->k0;
         BZ_GET_SMALL(k1); s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };
   
         s->state_out_len = 2;
         BZ_GET_SMALL(k1); s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };
   
         s->state_out_len = 3;
         BZ_GET_SMALL(k1); s->nblock_used++;
         if (s->nblock_used == s->save_nblock+1) continue;
         if (k1 != s->k0) { s->k0 = k1; continue; };
   
         BZ_GET_SMALL(k1); s->nblock_used++;
         s->state_out_len = ((Int32)k1) + 4;
         BZ_GET_SMALL(s->k0); s->nblock_used++;
      }

   }
}


/*---------------------------------------------------*/
#if 0
int BZ_API(BZ2_bzDecompress) ( bz_stream *strm )
{
   Bool    corrupt;
   DState* s;
   if (strm == NULL) return BZ_PARAM_ERROR;
   s = strm->state;
   if (s == NULL) return BZ_PARAM_ERROR;
   if (s->strm != strm) return BZ_PARAM_ERROR;

   while (True) {
      if (s->state == BZ_X_IDLE) return BZ_SEQUENCE_ERROR;
      if (s->state == BZ_X_OUTPUT) {
         if (s->smallDecompress)
            corrupt = unRLE_obuf_to_output_SMALL ( s ); else
            corrupt = unRLE_obuf_to_output_FAST  ( s );
         if (corrupt) return BZ_DATA_ERROR;
         if (s->nblock_used == s->save_nblock+1 && s->state_out_len == 0) {
            BZ_FINALISE_CRC ( s->calculatedBlockCRC );
            if (s->verbosity >= 3) 
               VPrintf2 ( " {0x%08x, 0x%08x}", s->storedBlockCRC, 
                          s->calculatedBlockCRC );
            if (s->verbosity >= 2) VPrintf0 ( "]" );
            if (s->calculatedBlockCRC != s->storedBlockCRC)
               return BZ_DATA_ERROR;
            s->calculatedCombinedCRC 
               = (s->calculatedCombinedCRC << 1) | 
                    (s->calculatedCombinedCRC >> 31);
            s->calculatedCombinedCRC ^= s->calculatedBlockCRC;
            s->state = BZ_X_BLKHDR_1;
         } else {
            return BZ_OK;
         }
      }
      if (s->state >= BZ_X_MAGIC_1) {
         Int32 r = BZ2_decompress ( s );
         if (r == BZ_STREAM_END) {
            if (s->verbosity >= 3)
               VPrintf2 ( "\n    combined CRCs: stored = 0x%08x, computed = 0x%08x", 
                          s->storedCombinedCRC, s->calculatedCombinedCRC );
            if (s->calculatedCombinedCRC != s->storedCombinedCRC)
               return BZ_DATA_ERROR;
            return r;
         }
         if (s->state != BZ_X_OUTPUT) return r;
      }
   }

   AssertH ( 0, 6001 );

   return 0;  /*NOTREACHED*/
}
#endif

/*---------------------------------------------------*/
int BZ_API(BZ2_bzDecompressEnd)  ( bz_stream *strm )
{
   DState* s;
   if (strm == NULL) return BZ_PARAM_ERROR;
   s = strm->state;
   if (s == NULL) return BZ_PARAM_ERROR;
   if (s->strm != strm) return BZ_PARAM_ERROR;

   if (s->tt   != NULL) BZFREE(s->tt);
   if (s->ll16 != NULL) BZFREE(s->ll16);
   if (s->ll4  != NULL) BZFREE(s->ll4);

   BZFREE(strm->state);
   strm->state = NULL;

   return BZ_OK;
}


#ifndef BZ_NO_STDIO
/*---------------------------------------------------*/
/*--- File I/O stuff                              ---*/
/*---------------------------------------------------*/

#define BZ_SETERR(eee)                    \
{                                         \
   if (bzerror != NULL) *bzerror = eee;   \
   if (bzf != NULL) bzf->lastErr = eee;   \
}

typedef 
   struct {
      FILE*     handle;
      Char      buf[BZ_MAX_UNUSED];
      Int32     bufN;
      Bool      writing;
      bz_stream strm;
      Int32     lastErr;
      Bool      initialisedOk;
   }
   bzFile;


/*---------------------------------------------*/
static Bool myfeof ( FILE* f )
{
   Int32 c = fgetc ( f );
   if (c == EOF) return True;
   ungetc ( c, f );
   return False;
}



#endif



#ifndef BZ_NO_STDIO
/*---------------------------------------------------*/

#if defined(_WIN32) || defined(OS2) || defined(MSDOS)
#   include <fcntl.h>
#   include <io.h>
#   define SET_BINARY_MODE(file) setmode(fileno(file),O_BINARY)
#else
#   define SET_BINARY_MODE(file)
#endif



/*---------------------------------------------------*/
/*--
   return last error code 
--*/
static char *bzerrorstrings[] = {
       "OK"
      ,"SEQUENCE_ERROR"
      ,"PARAM_ERROR"
      ,"MEM_ERROR"
      ,"DATA_ERROR"
      ,"DATA_ERROR_MAGIC"
      ,"IO_ERROR"
      ,"UNEXPECTED_EOF"
      ,"OUTBUFF_FULL"
      ,"CONFIG_ERROR"
      ,"???"   /* for future */
      ,"???"   /* for future */
      ,"???"   /* for future */
      ,"???"   /* for future */
      ,"???"   /* for future */
      ,"???"   /* for future */
};


const char * BZ_API(BZ2_bzerror) (BZFILE *b, int *errnum)
{
   int err = ((bzFile *)b)->lastErr;

   if(err>0) err = 0;
   *errnum = err;
   return bzerrorstrings[err*-1];
}
#endif


/*-------------------------------------------------------------*/
/*--- end                                           bzlib.c ---*/
/*-------------------------------------------------------------*/
void BZ2_compressBlock ( EState* s, Bool is_last_block ) {}
	
	
int BZ_API(BZ2_bzRead) ( int*    bzerror, BZFILE* b, void*   buf, int     len )
{
	Int32   n, ret;
	bzFile* bzf = (bzFile*)b;

	BZ_SETERR(BZ_OK);

	if (bzf == NULL || buf == NULL || len < 0)
	{ BZ_SETERR(BZ_PARAM_ERROR); return 0; };

	if (bzf->writing)
	{ BZ_SETERR(BZ_SEQUENCE_ERROR); return 0; };

	if (len == 0)
	{ BZ_SETERR(BZ_OK); return 0; };

	bzf->strm.avail_out = len;
	bzf->strm.next_out = buf;

	DState *s;
	s = bzf->strm.state;
	while (True) {

		if (ferror(bzf->handle))
		{ BZ_SETERR(BZ_IO_ERROR); return 0; };

		if (bzf->strm.avail_in == 0 && !myfeof(bzf->handle)) {
			n = fread ( bzf->buf, sizeof(UChar),
					BZ_MAX_UNUSED, bzf->handle );
			if (ferror(bzf->handle))
			{ BZ_SETERR(BZ_IO_ERROR); return 0; };
			bzf->bufN = n;
			bzf->strm.avail_in = bzf->bufN;
			bzf->strm.next_in = bzf->buf;
		}

		ret = BZ2_bzDecompress( &(bzf->strm) );

		if (ret != BZ_OK && ret != BZ_STREAM_END)
		{ BZ_SETERR(ret); return 0; };

		if (ret == BZ_OK && myfeof(bzf->handle) &&
				bzf->strm.avail_in == 0 && bzf->strm.avail_out > 0)
		{ BZ_SETERR(BZ_UNEXPECTED_EOF); return 0; };

		if (ret == BZ_STREAM_END)
		{ BZ_SETERR(BZ_STREAM_END);
			return len - bzf->strm.avail_out; };
			if (bzf->strm.avail_out == 0)
			{ BZ_SETERR(BZ_OK); return len; };

	}

	return 0; /*not reached*/
}

#if !defined(HAVE_LARGEFILE_SUPPORT)
typedef off_t Py_off_t;
#elif SIZEOF_OFF_T >= 8
typedef off_t Py_off_t;
#elif SIZEOF_FPOS_T >= 8
typedef fpos_t Py_off_t;
#else
#error "Large file support, but neither off_t nor fpos_t is large enough."
#endif
struct currentBlockInfos {
	 long long pos;

	 Py_off_t currentBlockByte;
	 int currentBlockBitOffset;

	 unsigned int blockStart_lo32; // offset in "decompressed" file
	 unsigned int blockStart_hi32; // offset in "decompressed" file
	 int nbblock;
};

int BZ_API(My_bzRead) ( int*    bzerror, BZFILE* b, void*   buf, int len, struct currentBlockInfos *blockInfos)
{
	Int32   n, ret;
	bzFile* bzf = (bzFile*)b;

	BZ_SETERR(BZ_OK);

	if (bzf == NULL || buf == NULL || len < 0)
	{ BZ_SETERR(BZ_PARAM_ERROR); return 0; };

	if (bzf->writing)
	{ BZ_SETERR(BZ_SEQUENCE_ERROR); return 0; };

	if (len == 0)
	{ BZ_SETERR(BZ_OK); return 0; };

	bzf->strm.avail_out = len;
	bzf->strm.next_out = buf;

	DState *s;
	s = bzf->strm.state;
	while (True) {

		if (ferror(bzf->handle))
		{ BZ_SETERR(BZ_IO_ERROR); return 0; };

		if (bzf->strm.avail_in == 0 && !myfeof(bzf->handle)) {
			n = fread ( bzf->buf, sizeof(UChar),
					BZ_MAX_UNUSED, bzf->handle );
			if (ferror(bzf->handle))
			{ BZ_SETERR(BZ_IO_ERROR); return 0; };
			bzf->bufN = n;
			bzf->strm.avail_in = bzf->bufN;
			bzf->strm.next_in = bzf->buf;
		}

		do {
			ret = BZ2_bzDecompressStepByStep ( &(bzf->strm) );
			if (s->state == BZ_X_BLKHDR_1) {
				/*
				   printf("%d\n", s->strm->avail_in);
				   printf("%d\n", ftell(bzf->handle));
				   printf("offset bit:%d\n", s->bsLive);
				   printf("bit: %d\n", (ftell(bzf->handle) - s->strm->avail_in + 6) * 8 - s->bsLive);
				   */
				int bits = 8 - s->bsLive;
				blockInfos->currentBlockByte = (ftello(bzf->handle) - (Py_off_t)s->strm->avail_in - 1);
				blockInfos->currentBlockBitOffset = bits;
				blockInfos->nbblock = s->currBlockNo;
				blockInfos->blockStart_lo32 = bzf->strm.total_out_lo32;
				blockInfos->blockStart_hi32 = bzf->strm.total_out_hi32;
			}
		} while (ret == BZ_X_BUSY);

		if (ret != BZ_OK && ret != BZ_STREAM_END)
		{ BZ_SETERR(ret); return 0; };

		if (ret == BZ_OK && myfeof(bzf->handle) &&
				bzf->strm.avail_in == 0 && bzf->strm.avail_out > 0)
		{ BZ_SETERR(BZ_UNEXPECTED_EOF); return 0; };

		if (ret == BZ_STREAM_END)
		{ BZ_SETERR(BZ_STREAM_END);
			return len - bzf->strm.avail_out; };
			if (bzf->strm.avail_out == 0)
			{ BZ_SETERR(BZ_OK); return len; };

	}

	return 0; /*not reached*/
}
/*---------------------------------------------------*/
int BZ2_bzDecompressStepByStep ( bz_stream *strm )
{
	Bool    corrupt;
	DState* s;
	if (strm == NULL) return BZ_PARAM_ERROR;
	s = strm->state;
	if (s == NULL) return BZ_PARAM_ERROR;
	if (s->strm != strm) return BZ_PARAM_ERROR;

	while (True) {
		//printf("Entering : %x %d\n", s->bsBuff, s->bsLive);
		if (s->state == BZ_X_IDLE) return BZ_SEQUENCE_ERROR;
		if (s->state == BZ_X_OUTPUT) {
			if (s->smallDecompress)
				corrupt = unRLE_obuf_to_output_SMALL ( s ); else
					corrupt = unRLE_obuf_to_output_FAST  ( s );
			if (corrupt) return BZ_DATA_ERROR;
			if (s->nblock_used == s->save_nblock+1 && s->state_out_len == 0) {
				BZ_FINALISE_CRC ( s->calculatedBlockCRC );
				if (s->verbosity >= 3)
					VPrintf2 ( " {0x%08x, 0x%08x}", s->storedBlockCRC,
							s->calculatedBlockCRC );
				if (s->verbosity >= 2) VPrintf0 ( "]" );
				if (s->calculatedBlockCRC != s->storedBlockCRC)
					return BZ_DATA_ERROR;
				s->calculatedCombinedCRC
					= (s->calculatedCombinedCRC << 1) |
					(s->calculatedCombinedCRC >> 31);
				s->calculatedCombinedCRC ^= s->calculatedBlockCRC;
				s->state = BZ_X_BLKHDR_1;
				return BZ_X_BUSY;
			} else {
				return BZ_OK;
			}
		}
		if (s->state >= BZ_X_MAGIC_1) {
			Int32 r = BZ2_decompress ( s );
			if (r == BZ_STREAM_END) {
				if (s->verbosity >= 3)
					VPrintf2 ( "\n    combined CRCs: stored = 0x%08x, computed = 0x%08x",
							s->storedCombinedCRC, s->calculatedCombinedCRC );
				if (s->calculatedCombinedCRC != s->storedCombinedCRC)
					return BZ_DATA_ERROR;
				return r;
			}
			if (s->state != BZ_X_OUTPUT) return r;
		}
		return BZ_X_BUSY;
	}

	AssertH ( 0, 6001 );

	return 0;  /*NOTREACHED*/
}

int BuffToBuffDecompressSeek(	char*         dest,
				unsigned int* destLen,
				char*         source,
				unsigned int  sourceLen,
				int           small,
			  char blockSizeChar,
				int bitOffset,
				int           verbosity
				)
{
	bz_stream strm;
	int ret;

	if (dest == NULL || destLen == NULL ||
			source == NULL ||
			(small != 0 && small != 1) ||
			verbosity < 0 || verbosity > 4)
		return BZ_PARAM_ERROR;

	strm.bzalloc = NULL;
	strm.bzfree = NULL;
	strm.opaque = NULL;
	ret = BZ2_bzDecompressInit ( &strm, verbosity, small );
	if (ret != BZ_OK) return ret;

	strm.next_in = source;
	strm.next_out = dest;
	strm.avail_in = sourceLen;
	strm.avail_out = *destLen;

	int i;
	ret = BZ_X_BUSY;
	int state;
	DState *s;
	s = strm.state;

	s->blockSize100k = blockSizeChar - '0';
	if (s->smallDecompress) {
					s->ll16 = malloc( s->blockSize100k * 100000 * sizeof(UInt16) );
					s->ll4  = malloc(
								((1 + s->blockSize100k * 100000) >> 1) * sizeof(UChar));
					if (s->ll16 == NULL || s->ll4 == NULL) return(BZ_MEM_ERROR);
	} else {
					s->tt  = malloc( s->blockSize100k * 100000 * sizeof(Int32) );
					if (s->tt == NULL) return(BZ_MEM_ERROR);
	}

		s->state = BZ_X_BLKHDR_1;

		s->bsBuff = (UInt32)(*((UChar*)(strm.next_in)));

		s->bsLive = 8 - bitOffset;

		int offset = 1;
		strm.total_in_lo32 += offset;
		strm.avail_in -= offset;
		strm.next_in += offset;

		//s->bsBuff = ((UInt32)(*((UChar*)(strm.next_in - 1))));
		//s->bsBuff = 0xCC;
//		printf("(%x)\n", s->bsBuff);
		// 
//printf("ready at buf : %i, %d\n", strm.total_in_lo32,s->bsLive);
//printf("avail at buf : %i, %p\n", strm.avail_in,strm.next_in);

  int nb = 1;
	for (i = 0; ret == BZ_X_BUSY; i++) {
		/*printf ("Start! %d..\n", i);
			printf(" ..state = %d\n", s->state);
		*/
		ret = BZ2_bzDecompressStepByStep ( &strm );
			if (s->state == BZ_X_OUTPUT) {
				nb --;
				//printf("SORS UNE FOIS! state=%d\n", s->state);
				//printf("buf : %i, %d\n", strm.total_in_lo32,s->bsLive);
					ret = BZ2_bzDecompressStepByStep ( &strm );
				if (nb == 0)
					ret = BZ_STREAM_END;
			}
	}

	if (ret == BZ_OK) goto output_overflow_or_eof;
	if (ret != BZ_STREAM_END) goto errhandler;

	/* normal termination */
	*destLen -= strm.avail_out;
	BZ2_bzDecompressEnd ( &strm );
	return BZ_OK;

output_overflow_or_eof:
	if (strm.avail_out > 0) {
		BZ2_bzDecompressEnd ( &strm );
		return BZ_UNEXPECTED_EOF;
	} else {
		BZ2_bzDecompressEnd ( &strm );
		return BZ_OUTBUFF_FULL;
	};

errhandler:
	BZ2_bzDecompressEnd ( &strm );
	return ret;
}

char* BZ_API(loadBlock) ( BZFILE* b, long long ByteOffset, int bitOffset, unsigned int *outlen)
{
	bzFile* bzf = (bzFile*)b;
	DState* s = bzf->strm.state;
	FILE *f = bzf->handle;

	char *inbuf = malloc(1024*1024);
	char *outbuf = malloc(1024*1024);
	unsigned int nOut = 1024*1024;
	unsigned int nIn;
	int r;
	//printf("Seeking........\n");
  fseeko(f, ByteOffset, SEEK_SET);
	//printf("Reading........\n");
  nIn = fread ( inbuf, 1, 1024*1024, f );
	 
  r = BuffToBuffDecompressSeek( outbuf, &nOut, inbuf, nIn, True, '9', bitOffset, 0 );
	*outlen = nOut;
	free(inbuf);
 if (r != BZ_OK) {
	free(outbuf);
	return NULL;
  }
  assert (r == BZ_OK);
 
	return outbuf;
}
int main ( int argc, char** argv )
{
   FILE* f;
   FILE *o;
   char inbuf[1024*1024*3]; // input block + header
   char outbuf[1024*900*3];
   int   r;
   int   i;
   unsigned int nOut = sizeof(outbuf), nIn;

   if (argc != 2) {
      printf (  "usage: unzcrash filename\n" );
      return 1;
   }

   f = fopen ( argv[1], "r" );
   if (!f) {
      printf (  "unzcrash: can't open %s\n", argv[1] );
      return 1;
   }

	 // x = 1er col de bz2recover
	 // bit = x % 8
	 // Byte = (x - bit) / 8 - 6
	 int bitOffset = 4;
	 int ByteOffset = 141466748;
   fseek(f, ByteOffset, SEEK_SET);
   nIn = fread ( inbuf, 1, sizeof(inbuf), f );
   fclose(f);
	 
   r = BuffToBuffDecompressSeek( outbuf, &nOut, inbuf, nIn, True, '9', bitOffset, 0 );
   assert (r == BZ_OK);
   o = fopen("output", "w");
   printf (  "%d after decompression\n", nOut );
   fwrite(outbuf, nOut, 1, o);
   fclose(o);

   printf (  "all ok\n" );
   return 0;
}
