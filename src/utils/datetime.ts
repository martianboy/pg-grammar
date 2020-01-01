/* ----------------------------------------------------------------
 *				time types + support macros
 *
 * String definitions for standard time quantities.
 *
 * These strings are the defaults used to form output time strings.
 * Other alternative forms are hardcoded into token tables in datetime.c.
 * ----------------------------------------------------------------
 */

export const DAGO = "ago";
export const DCURRENT = "current";
export const EPOCH = "epoch";
export const INVALID = "invalid";
export const EARLY = "-infinity";
export const LATE = "infinity";
export const NOW = "now";
export const TODAY = "today";
export const TOMORROW = "tomorrow";
export const YESTERDAY = "yesterday";
export const ZULU = "zulu";

export const DMICROSEC = "usecond";
export const DMILLISEC = "msecond";
export const DSECOND = "second";
export const DMINUTE = "minute";
export const DHOUR = "hour";
export const DDAY = "day";
export const DWEEK = "week";
export const DMONTH = "month";
export const DQUARTER = "quarter";
export const DYEAR = "year";
export const DDECADE = "decade";
export const DCENTURY = "century";
export const DMILLENNIUM = "millennium";
export const DA_D = "ad";
export const DB_C = "bc";
export const DTIMEZONE = "timezone";

/*
 * Fundamental time field definitions for parsing.
 *
 *	Meridian:  am, pm, or 24-hour style.
 *	Millennium: ad, bc
 */

export const AM = 0;
export const PM = 1;
export const HR24 = 2;

export const AD = 0;
export const BC = 1;

/*
 * Field types for time decoding.
 *
 * Can't have more of these than there are bits in an unsigned int
 * since these are turned into bit masks during parsing and decoding.
 *
 * Furthermore, the values for YEAR, MONTH, DAY, HOUR, MINUTE, SECOND
 * must be in the range 0..14 so that the associated bitmasks can fit
 * into the left half of an INTERVAL's typmod value.  Since those bits
 * are stored in typmods, you can't change them without initdb!
 */

export const RESERV = 0;
export const MONTH = 1;
export const YEAR = 2;
export const DAY = 3;
export const JULIAN = 4;
export const TZ = 5 /* fixed-offset timezone abbreviation */;
export const DTZ = 6 /* fixed-offset timezone abbrev, DST */;
export const DYNTZ = 7 /* dynamic timezone abbreviation */;
export const IGNORE_DTF = 8;
export const AMPM = 9;
export const HOUR = 10;
export const MINUTE = 11;
export const SECOND = 12;
export const MILLISECOND = 13;
export const MICROSECOND = 14;
export const DOY = 15;
export const DOW = 16;
export const UNITS = 17;
export const ADBC = 18;
/* these are only for relative dates */
export const AGO = 19;
export const ABS_BEFORE = 20;
export const ABS_AFTER = 21;
/* generic fields to help with parsing */
export const ISODATE = 22;
export const ISOTIME = 23;
/* these are only for parsing intervals */
export const WEEK = 24;
export const DECADE = 25;
export const CENTURY = 26;
export const MILLENNIUM = 27;
/* hack for parsing two-word timezone specs "MET DST" etc */
export const DTZMOD = 28 /* "DST" as a separate word */;
/* reserved for unrecognized string values */
export const UNKNOWN_FIELD = 31;

/*
 * Token field definitions for time parsing and decoding.
 *
 * Some field type codes (see above) use these as the "value" in datetktbl[].
 * These are also used for bit masks in DecodeDateTime and friends
 *	so actually restrict them to within [0,31] for now.
 * - thomas 97/06/19
 * Not all of these fields are used for masks in DecodeDateTime
 *	so allow some larger than 31. - thomas 1997-11-17
 *
 * Caution: there are undocumented assumptions in the code that most of these
 * values are not equal to IGNORE_DTF nor RESERV.  Be very careful when
 * renumbering values in either of these apparently-independent lists :-(
 */

export const DTK_NUMBER = 0;
export const DTK_STRING = 1;

export const DTK_DATE = 2;
export const DTK_TIME = 3;
export const DTK_TZ = 4;
export const DTK_AGO = 5;

export const DTK_SPECIAL = 6;
export const DTK_EARLY = 9;
export const DTK_LATE = 10;
export const DTK_EPOCH = 11;
export const DTK_NOW = 12;
export const DTK_YESTERDAY = 13;
export const DTK_TODAY = 14;
export const DTK_TOMORROW = 15;
export const DTK_ZULU = 16;

export const DTK_DELTA = 17;
export const DTK_SECOND = 18;
export const DTK_MINUTE = 19;
export const DTK_HOUR = 20;
export const DTK_DAY = 21;
export const DTK_WEEK = 22;
export const DTK_MONTH = 23;
export const DTK_QUARTER = 24;
export const DTK_YEAR = 25;
export const DTK_DECADE = 26;
export const DTK_CENTURY = 27;
export const DTK_MILLENNIUM = 28;
export const DTK_MILLISEC = 29;
export const DTK_MICROSEC = 30;
export const DTK_JULIAN = 31;

export const DTK_DOW = 32;
export const DTK_DOY = 33;
export const DTK_TZ_HOUR = 34;
export const DTK_TZ_MINUTE = 35;
export const DTK_ISOYEAR = 36;
export const DTK_ISODOW = 37;

/*
 * Bit mask definitions for time parsing.
 */

export const DTK_M = (t: number) => 0x01 << t;

/* Convenience: a second, plus any fractional component */
export const DTK_ALL_SECS_M =
  DTK_M(SECOND) | DTK_M(MILLISECOND) | DTK_M(MICROSECOND);
export const DTK_DATE_M = DTK_M(YEAR) | DTK_M(MONTH) | DTK_M(DAY);
export const DTK_TIME_M = DTK_M(HOUR) | DTK_M(MINUTE) | DTK_ALL_SECS_M;
