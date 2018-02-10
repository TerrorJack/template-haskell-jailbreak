#include <stdio.h>
#include <alloca.h>
#include <sys/stat.h>
#include <unistd.h>

int pargs(void *argv) {
  void *path_buf = alloca(32);
  if (snprintf(path_buf, 32, "/proc/%d/cmdline", getppid()) < 0) return 1;
  int fd = open(path_buf, O_RDONLY);
  if (fd == -1) return 1;
  if (read(fd, argv, 1024576) < 0) return 1;
  if (close(fd) != 0) return 1;
  return 0;
}
