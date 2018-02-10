#include <alloca.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

int pargs(void *argv) {
  void *path_buf = alloca(32);
  if (snprintf(path_buf, 32, "/proc/%d/cmdline", getppid()) < 0) return 1;
  int fd = open(path_buf, O_RDONLY);
  if (fd == -1) return 1;
  memset(argv, 0, 65536);
  if (read(fd, argv, 65536) < 0) return 1;
  if (close(fd) != 0) return 1;
  return 0;
}
