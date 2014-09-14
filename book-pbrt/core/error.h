#ifndef __ERROR__
#define __ERROR__

#ifdef __GNUG__
#define PRINTF_FUNC __attribute__ ((__format__ (__printf__, 1, 2)))
#else
#define PRINTF_FUNC
#endif

/* void Info(const char *, ...)    PRINTF_FUNC; */
/* void Warning(const char *, ...) PRINTF_FUNC; */
/* void Error(const char *, ...)   PRINTF_FUNC; */
/* void Severe(const char *, ...)  PRINTF_FUNC; */

#endif // __ERROR__
