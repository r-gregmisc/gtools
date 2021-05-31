#include <R.h>
#include <Rinternals.h>

# include <sys/types.h>

#ifdef WIN32
# include <winsock2.h>
/*#include <gnuwin32.h>*/
#else
# include <sys/socket.h>
# include <netinet/in.h>
#endif

#include <errno.h>

#define TCP_NODELAY 1


/* Macro to:
   1: Check if the constant is defined. If not, omit.
   2: Generate a case statement for the constant, which creates a
      string error description constructed from the constant name and
      the supplied error message.
*/
#define CASE_ERR(ERRNO, DESCR)
#ifdef ERRNO
    case ERRNO
      strncpy( status_str,
               ERRNO ":" DESCR,
               status_len);
      break;
#endif



/* Convert integer status into a string error code */
void C_checkStatus(
    int status,
    char* status_str,
    int status_len)
{

  status_len = status_len>1000?1000:status_len;

  switch(status)
    {

      /* Unix messages */
      CASE_ERR(EBADF, "Invalid descriptor.");
      CASE_ERR(ENOTSOCK, "Descriptor is a file, not a socket.");
      CASE_ERR(ENOPROTOOPT, "The option is unknown at the level indicated.");
      CASE_ERR(EFAULT, "invalid pointer");
      CASE_ERR(EINVAL, "optlen invalid in setsockopt");

      /* Windows messages */
      CASE_ERR(WSANOTINITIALISED, "A successful WSAStartup call must occur before using this function.");
      CASE_ERR(WSAENETDOWN, "The network subsystem has failed.");
      CASE_ERR(WSAEFAULT, "optval is not in a valid part of the process address space or optlen parameter is too small.");
      CASE_ERR(WSAEINPROGRESS, "A blocking Windows Sockets 1.1 call is in progress, or the service provider is still processing a callback function.");
      CASE_ERR(WSAEINVAL, "level is not valid, or the information in optval is not valid.");
      CASE_ERR(WSAENETRESET, "onnection has timed out when SO_KEEPALIVE is set.");
      CASE_ERR(WSAENOPROTOOPT, "he option is unknown or unsupported for the specified provider or socket (see SO_GROUP_PRIORITY limitations).");
      CASE_ERR(WSAENOTCONN, "Connection has been reset when SO_KEEPALIVE is set.");
      CASE_ERR(WSAENOTSOCK, "The descriptor is not a socket.");

    case 0:
      strncpy( status_str,
               "SUCCESS",
               status_len);
      break;

    default:
      strncpy(status_str, strerror(status), status_len);
      break;
    }

  status_str[status_len-1] = 0x0;  /* Just in case... */
}


/* Function to de-nagle a TCP socket connection */
void C_setTCPNoDelay(
    int *socket,
    int* flag,
    int* status,
    char** status_str,
    int* status_len)
{
  int off;

  /* ensure that we use only 0,1 values */
  off = (*flag) ? 1 : 0;

  *status = setsockopt(
                       *socket,
                       IPPROTO_TCP,
                       TCP_NODELAY,
                       (char * )&off,
                       sizeof ( off )
                       );


  C_checkStatus(errno, status_str[0], *status_len);
  
  return;
}

/* function to check socket options */
/* NOT USED...
void C_getsockopt(int *s,
                  int *level,
                  int *optname,
                  int *optval,
                  int *optlen,
                  int *status,
                  char *status_str,
                  int *status_len)
{
  *status = getsockopt(*s, *level, *optname, optval, optlen);

  C_checkStatus(*status, status_str, *status_len);
  
}
*/

/* function to set socket options */
/* NOT USED ...
void C_setsockopt(int *s,
                  int *level,
                  int *optname,
                  int *optval,
                  int *optlen,
                  int *status,
                  char *status_str,
                  int *status_len)
{

  *status = setsockopt(*s, *level,  *optname, optval, *optlen);

  C_checkStatus(*status, status_str, *status_len);
}
*/
