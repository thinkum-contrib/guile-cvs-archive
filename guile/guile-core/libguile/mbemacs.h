#ifndef SCM_MBEMACS_H
#define SCM_MBEMACS_H

/* A description of Guile's internal text encoding, from Emacs 20.4.

	  Copyright (C) 1999 Free Software Foundation, Inc.
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this software; see the file COPYING.  If not, write to
   the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
   Boston, MA 02111-1307 USA
  
   As a special exception, the Free Software Foundation gives permission
   for additional uses of the text contained in its release of GUILE.
  
   The exception is that, if you link the GUILE library with other files
   to produce an executable, this does not by itself cause the
   resulting executable to be covered by the GNU General Public License.
   Your use of that executable is in no way restricted on account of
   linking the GUILE library code into it.
  
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.
  
   This exception applies only to the code released by the
   Free Software Foundation under the name GUILE.  If you copy
   code from other Free Software Foundation releases into a copy of
   GUILE, as the General Public License permits, the exception does
   not apply to the code that you add in this way.  To avoid misleading
   anyone as to the status of such modified files, you must delete
   this exception notice from them.
  
   If you write modifications of your own for GUILE, it is your choice
   whether to permit this exception to apply to your modifications.
   If you do not wish that, delete this exception notice.  */


/* A general description of the encoding.  */

/* ** THIS WILL CHANGE IN FUTURE VERSIONS OF GUILE --- IT IS NOT
   CORRECT TO ASSUME THE USE OF THIS ENCODING OUTSIDE OF mb.c AND
   mb.h, AND THE ENCODING CONVERSION FUNCTIONS. **

   If you can't accomplish what you want without this info, then the
   multibyte API is flawed, and we need to extend it.  If you spread
   this knowledge around into other code, then it will break when we
   change encodings.

   You have been warned.


   For all ASCII characters, the Guile scm_char_t code is equal to the
   ASCII code.  The Guile multi-byte encoding is a single byte whose
   value is the character's ASCII code.  Note that ASCII doesn't
   contain any characters whose numbers are above 127.

   For non-ASCII characters:

   Each character is assigned a character set number from 0x81 to 0xFE
   (except for 0x9A .. 0x9F), and a position within that character
   set.  A "position" within a character set is one or two bytes from
   0x20 to 0x7F.

   For example:
   - The Latin-1 character set is 0x81.
   - The Japanese JISX0208.1983/1990 Kanji set is 0x92.
   - The character á (a lower-case a with an acute accent) is part of
     the Latin-1 character set.  Its position is the byte 0x61.
   - The Japanese Katakana character "ka" is part of the JISX0208
     character set.  Its position is the pair of bytes 0x25 0x2B.

   Once we know a character's character set, we can determine whether
   its position is one or two bytes, and the form of its encoding.

   Character set number     positions 	 encoding byte sequence
   ===========================================================================
   from 0x81 to 0x8f        1 byte    	 SET   POS +0x80
   from 0x90 to 0x99        2 bytes   	 SET   POS1+0x80  POS2+0x80
   from 0xA0 to 0xDF        1 byte    	 0x9A  SET        POS +0x80
   from 0xE0 to 0xEF        1 byte    	 0x9B  SET        POS +0x80
   from 0xF0 to 0xF4        2 bytes   	 0x9C  SET        POS1+0x80  POS2+0x80
   from 0xF5 to 0xFE        2 bytes   	 0x9D  SET        POS1+0x80  POS2+0x80

   "SET" is the character set number;
   "POS" is a one-byte position; and
   "POS1" and "POS2" are a two-byte position, 

   Some examples:
   - For the character á, SET is 0x81, and POS is 0x61, so it would be
     encoded by the byte sequence 0x81 0xE1.
   - For the Japanese Katakana character "ka", SET is 0x92, and POS1
     and POS2 are 0x25 and 0x2B, so it would be encoded by the byte
     sequence 0x92 0xA5 0xAB.

   So the longest encoding is four bytes long.

   It's easy to verify that this encoding meets the conditions
   promised by mbapi.texi:

   - Every ASCII character is encoded as a single byte from 0 to 127,
     in the obvious way.
   - The encodings of non-ASCII characters use only bytes between 0x80
     and 0xFF.
   - No character encoding is a subsequence of any other character
     encoding, since bytes from 0x00 to 0x9f occur only at the
     beginning of a sequence.
   - You can always determine the full length of a character's
     encoding from its first byte.
   - Given an arbitrary byte position in a Guile string, you can
     always find the beginning and end of the character containing
     that byte without scanning too far in either direction, assuming
     the string is null-terminated or followed by another valid
     character (as substrings are).


   How does Guile choose scm_char_t values for non-ASCII characters?

   We divide a character value up into three fields:
   FIELD1: bits 18 -- 14  (most significant bits)
   FIELD2: bits 13 --  7
   FIELD3: bits  6 --  0  (least significant bits)

   If the character's position is one byte, then:
     FIELD1 is zero.
     FIELD2 is the character set number, minus 0x70.
     FIELD3 is the character position.
   
   If the character's position is two bytes, then:
     FIELD2 is the first byte of the character's position.
     FIELD3 is the second byte of the character's position.
     If the character set number is from 0x90 to 0x99, then:
       FIELD1 is the character set number, minus 0x8f.
       (Thus, a number from 0x01 to 0x0A.)
     If the character set number is from 0xF0 to 0xFE, then:
       FIELD1 is the character set number, minus 0xE0.
       (Thus, a number from 0x10 to 0x1E.)

   For example:
   - For the character á, FIELD1 would be zero, FIELD2 would be 0x11,
     and FIELD3 would be 0x61.  Thus, the full character code would be
     (0x11 << 7) | 0x61, or 2273.
   - For the Japanese Katakana character "ka", FIELD1 would be 0x3,
     FIELD2 would be 0x25 and FIELD3 would be 0x2B.  Thus, the full
     character code would be (0x3 << 14) | (0x25 << 7) | 0x2B, or 53931.

   Thus, character codes fall into the following ranges:

         0 ..    127    ASCII
      2208 ..   4095    "official"   one-byte position character sets
      6176 ..  16383    "unofficial" one-byte position character sets
     20512 .. 180223    "official"   two-byte position character sets
    266272 .. 507903    "unofficial" two-byte position character sets

   It's hairy, but at the time this was designed, Unicode didn't exist
   --- this encoding allowed Emacs to incorporate characters from all
   kinds of character sets unchanged.  It also allows Emacs to
   distinguish between Japanese and Chinese character sets, which is
   important to some users.

   Even when we make the transition to Unicode, we will probably
   retain some way of distinguishing Japanese and Chinese characters.
   This is a highly controvertial issue.  However, I think that the
   opinions of people who do not use Chinese or Japanese regularly
   should be discounted; once this is done, there is a substantial
   body of users who say they need this distinction in the encoding
   itself.  So Guile will support it.  */



/* Assembling and disassembling character codes.  */

/* A `CHAR1' is a character whose position is one byte.
   A `CHAR2' is a character whose position is two bytes.
   The suffix `O' refers to an "official" character set --- one
       whose character set number is in the range 0x81 -- 0x99.
   The suffix `P' refers to a "private" character set --- one
       whose character set number is in the range 0xA0 -- 0xFE.
*/

#define BUILD_CHAR1(set, pos) ((((set) - 0x70) << 7) | (pos))
#define BUILD_CHAR2(set, offset, pos1, pos2)	\
  ((((set) - (offset)) << 14) 			\
   | ((pos1) << 7)				\
   | (pos2))

#define IS_ASCII_CHAR(c) ((unsigned) (c) < 0x80)

#define FIRST_CHAR1O (BUILD_CHAR1 (0x81, 0x20))
#define LAST_CHAR1O  (BUILD_CHAR1 (0x8f, 0x7F))

#define FIRST_CHAR1P (BUILD_CHAR1 (0xA0, 0x20))
#define LAST_CHAR1P  (BUILD_CHAR1 (0xEF, 0x7F))

#define FIRST_CHAR2O (BUILD_CHAR2 (0x90, 0x8F, 0x20, 0x20))
#define LAST_CHAR2O  (BUILD_CHAR2 (0x99, 0x8F, 0x7F, 0x7F))

#define FIRST_CHAR2P (BUILD_CHAR2 (0xF0, 0xE0, 0x20, 0x20))
#define LAST_CHAR2P  (BUILD_CHAR2 (0xFE, 0xE0, 0x7F, 0x7F))

#define CHAR1_SET(c) (((c) >> 7) + 0x70)
#define CHAR1_POS(c) ((c) & 0x7F)

#define CHAR2_SET(c, offset) (((c) >> 14) + (offset))
#define CHAR2_POS1(c) (((c) >> 7) & 0x7f)
#define CHAR2_POS2(c) ((c) & 0x7f)


/* Character set numbers.  */

/* These are extracted from Emacs 20.4.  */

#define CHARSET_ASCII (000)		     /* ASCII (ISO646 IRV) */
#define CHARSET_LATIN_ISO8859_1 (129)	     /* ISO8859-1 (Latin-1) */
#define CHARSET_LATIN_ISO8859_2 (130)	     /* ISO8859-2 (Latin-2) */
#define CHARSET_LATIN_ISO8859_3 (131)	     /* ISO8859-3 (Latin-3) */
#define CHARSET_LATIN_ISO8859_4 (132)	     /* ISO8859-4 (Latin-4) */
#define CHARSET_THAI_TIS620 (133)	     /* TIS620.2529 (Thai) */
#define CHARSET_GREEK_ISO8859_7 (134)	     /* ISO8859-7 (Greek) */
#define CHARSET_ARABIC_ISO8859_6 (135)	     /* ISO8859-6 (Arabic) */
#define CHARSET_HEBREW_ISO8859_8 (136)	     /* ISO8859-8 (Hebrew) */
#define CHARSET_KATAKANA_JISX0201 (137)	     /* JISX0201.1976 Japanese Kana */
#define CHARSET_LATIN_JISX0201 (138)	     /* JISX0201.1976 Japanese Roman */
#define CHARSET_CYRILLIC_ISO8859_5 (140)     /* ISO8859-5 (Cyrillic) */
#define CHARSET_LATIN_ISO8859_9 (141)	     /* ISO8859-9 (Latin-5) */
#define CHARSET_JAPANESE_JISX0208_1978 (144) /* JISX0208.1978 Japanese Kanji (so called "old JIS") */
#define CHARSET_CHINESE_GB2312 (145)	     /* GB2312 Chinese simplified */
#define CHARSET_JAPANESE_JISX0208 (146)	     /* JISX0208.1983/1990 Japanese Kanji */
#define CHARSET_KOREAN_KSC5601 (147)	     /* KSC5601 Korean Hangul and Hanja */
#define CHARSET_JAPANESE_JISX0212 (148)	     /* JISX0212 Japanese supplement */
#define CHARSET_CHINESE_CNS11643_1 (149)     /* CNS11643 Plane 1 Chinese traditional */
#define CHARSET_CHINESE_CNS11643_2 (150)     /* CNS11643 Plane 2 Chinese traditional */
#define CHARSET_CHINESE_BIG5_1 (152)	     /* Big5 Level-1 Chinese traditional */
#define CHARSET_CHINESE_BIG5_2 (153)	     /* Big5 Level-2 Chinese traditional */
#define CHARSET_CHINESE_SISHENG (160)	     /* SiSheng characters for PinYin/ZhuYin */
#define CHARSET_IPA (161)		     /* IPA (International Phonetic Association) */
#define CHARSET_VIETNAMESE_VISCII_LOWER (162) /* VISCII1.1 lower-case */
#define CHARSET_VIETNAMESE_VISCII_UPPER (163) /* VISCII1.1 upper-case */
#define CHARSET_ARABIC_DIGIT (164)	     /* Arabic digit */
#define CHARSET_ARABIC_1_COLUMN (165)	     /* Arabic 1-column */
#define CHARSET_ASCII_RIGHT_TO_LEFT (166)    /* ASCII (left half of ISO8859-1) with right-to-left direction */
#define CHARSET_LAO (167)		     /* Lao characters (ISO10646 0E80..0EDF) */
#define CHARSET_ARABIC_2_COLUMN (224)	     /* Arabic 2-column */
#define CHARSET_INDIAN_IS13194 (225)	     /* Generic Indian charset for data exchange with IS 13194 */
#define CHARSET_INDIAN_1_COLUMN (240)	     /* Indian charset for 2-column width glyphs */
#define CHARSET_TIBETAN_1_COLUMN (241)	     /* Tibetan 1 column glyph */
#define CHARSET_ETHIOPIC (245)		     /* Ethiopic characters */
#define CHARSET_CHINESE_CNS11643_3 (246)     /* CNS11643 Plane 3 Chinese Traditional */
#define CHARSET_CHINESE_CNS11643_4 (247)     /* CNS11643 Plane 4 Chinese Traditional */
#define CHARSET_CHINESE_CNS11643_5 (248)     /* CNS11643 Plane 5 Chinese Traditional */
#define CHARSET_CHINESE_CNS11643_6 (249)     /* CNS11643 Plane 6 Chinese Traditional */
#define CHARSET_CHINESE_CNS11643_7 (250)     /* CNS11643 Plane 7 Chinese Traditional */
#define CHARSET_INDIAN_2_COLUMN (251)	     /* Indian charset for 2-column width glyphs */
#define CHARSET_TIBETAN (252)		     /* Tibetan characters */


#endif /* SCM_MBEMACS_H */


