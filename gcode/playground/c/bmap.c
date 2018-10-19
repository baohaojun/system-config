/**** start of bhj auto includes ****/
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <linux/fs.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
/**** end of bhj auto includes ****/

#include <stdio.h>
int main(int argc, char *argv[])
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s FILE\n", argv[0]);
        exit(1);
    }

    int fd = open(argv[1], O_RDONLY);
    if (fd < 0) {
        fprintf(stderr, "Error: can't open %s: %s\n", argv[1], strerror(errno));
        exit(1);
    }
    struct stat st;
    if (fstat(fd, &st) < 0) {
        fprintf(stderr, "Error: can't stat %s: %s\n", argv[1], strerror(errno));
        exit(1);
    }
    unsigned long blksize = st.st_blksize;
    unsigned long filesize = st.st_size;

    if (filesize == 0) {
        return 0;
    }

    unsigned long n = (filesize + blksize - 1) / blksize;
    unsigned long i;
    printf("%s ", argv[1]);
    for (i = 0; i < n; i++) {
        if (i != 0) {
            i = n - 1;
        }

        unsigned long blknum = i;
        unsigned long fs_blknum;

        /*
         * FIBMAP takes a block index as input and on return replaces it with a
         * block number relative to the beginning of the filesystem/partition.
         * An output value of zero means "unallocated", or a "hole" in a sparse file.
         * Note that this is a 32-bit value on 32-bit systems, so it will not work
         * properly on files/filesystems with more than 4 billion blocks (~16TB),
         */
        if (ioctl(fd, FIBMAP, &blknum) == -1) {
            int err = errno;
            perror("ioctl(FIBMAP)");
            close(fd);
            return err;
        }
        fs_blknum = blknum;     /* work in 64-bits as much as possible */
        if (i != 0) {
            printf("-");
        }
        printf("%ld", fs_blknum);
    }
    printf("\n");
    close(fd);
    return 0;
}
