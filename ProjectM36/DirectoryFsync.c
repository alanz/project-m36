#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <strings.h>
#include <errno.h>
#include <fcntl.h>

#ifndef _DIRECTORY_FSYNC_
#define _DIRECTORY_FSYNC_
/* open a directory for reading and fsync the resultant fd */
int cDirectoryFsync(char *path)
{
  int fd = 0;
  int ret = 0;
  struct stat fdstat = {0};

  fd = open(path, O_RDONLY);
  if(fd < 0)
    {
      return errno;
    }

  /* ensure that opened fd is a directory fd. Otherwise, fsync will fail (on a read-only file descriptor */
  ret = fstat(fd,&fdstat);
  if(ret < 0)
    {
      return errno;
    }

  if(!S_ISDIR(fdstat.st_mode))
    {
      return ENOTDIR;
    }
  
  /* execute the fsync */
  ret = fsync(fd);
  if(ret < 0)
    {
      return errno;
    }

  return 0;
}
#endif
