
#include <stdio.h>
#include <string.h>
#include <winsock2.h>
#include <windows.h>
#include "flexdll.h"

 #define WIN32_LEAN_AND_MEAN
#include <wtypes.h>
#include <winbase.h>
#include <stdlib.h>
#include <direct.h>
#include <process.h>
#include <sys/types.h>
#include <ws2tcpip.h>
#include <wspiapi.h>
#include <signal.h>

#ifdef _WIN32

int WINAPI ocp_getnameinfo (const struct sockaddr *sa,socklen_t salen,char *host,size_t hostlen,char *serv,size_t servlen,int flags)
{
  return getnameinfo(sa,salen,host,hostlen,serv,servlen,flags);
}

int WINAPI ocp_getaddrinfo(const char *nodename,const char *servname,const struct addrinfo *hints,struct addrinfo **res)
{
  return getaddrinfo(nodename,servname,hints,res);
}
void WINAPI ocp_freeaddrinfo (struct addrinfo *ai)
{ freeaddrinfo(ai); }

typedef void (*sighandler)(int sig);
sighandler ocp_signal(int sig, sighandler action)
{
   return signal(sig, action);
}

#ifdef _MSC_VER 

///int ocp_main(int argc, char** argv);
void __main(){}

#include < time.h >
#include <windows.h> //I've ommited this line.
#if defined(_MSC_VER) || defined(_MSC_EXTENSIONS)
  #define DELTA_EPOCH_IN_MICROSECS  11644473600000000Ui64
#else
  #define DELTA_EPOCH_IN_MICROSECS  11644473600000000ULL
#endif
 
struct timezone 
{
  int  tz_minuteswest; /* minutes W of Greenwich */
  int  tz_dsttime;     /* type of dst correction */
};
 
int gettimeofday(struct timeval *tv, struct timezone *tz)
{
  FILETIME ft;
  unsigned __int64 tmpres = 0;
  static int tzflag;
 
  if (NULL != tv)
  {
    GetSystemTimeAsFileTime(&ft);
 
    tmpres |= ft.dwHighDateTime;
    tmpres <<= 32;
    tmpres |= ft.dwLowDateTime;
 
    /*converting file time to unix epoch*/
    tmpres -= DELTA_EPOCH_IN_MICROSECS; 
    tmpres /= 10;  /*convert into microseconds*/
    tv->tv_sec = (long)(tmpres / 1000000UL);
    tv->tv_usec = (long)(tmpres % 1000000UL);
  }
 
  if (NULL != tz)
  {
    if (!tzflag)
    {
      _tzset();
      tzflag++;
    }
    tz->tz_minuteswest = _timezone / 60;
    tz->tz_dsttime = _daylight;
  }
 
  return 0;
}

#endif // _MSC_VER

#endif // _WIN32
