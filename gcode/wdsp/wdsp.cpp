#include <stdio.h>
#include <stdlib.h>
#include <sys/soundcard.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <unistd.h>

int dsp_fd;
char *prog_name;

int open_dsp(int ulSamplesPerSec, 
         int uChannels, 
         int uBitsPerSample)
{
    dsp_fd = open("/dev/dsp", O_RDWR);
    if (dsp_fd < 0) {
        printf("%s: open /dev/dsp failed: %s\n", prog_name, strerror(errno));
        exit(-1);
    }

    int arg = ulSamplesPerSec;
    int status = ioctl(dsp_fd, SOUND_PCM_WRITE_RATE, &arg);
    if (status < 0) {
        perror("error from SOUND_PCM_WRITE_RATE ioctl");
        exit(1);
    }

    arg = uChannels;
    status = ioctl(dsp_fd, SOUND_PCM_WRITE_CHANNELS, &arg);
    if (status < 0) {
        perror("error from SOUND_PCM_WRITE_CHANNELS ioctl");
        exit(1);
    }

    arg = uBitsPerSample;
    status = ioctl(dsp_fd, SOUND_PCM_WRITE_BITS, &arg);
    if (status < 0) {
        perror("error from SOUND_PCM_WRITE_BITS ioctl");
        exit(1);
    }
    return 0;
}

void Usage()
{
    printf("Usage: wdsp -s sample_rate -c channels -b bits/sample file\n");
    exit(0);
}

void check_1_more_arg(int i, int argc)
{
    if (i >= argc) {
        Usage();
    }
}

#define debug(fmt, ...) do {    \
        fprintf(stderr,				\
                "%s %s() %d: " fmt "\n",        \
                strrchr(__FILE__, '/')          \
                ? strrchr(__FILE__, '/') + 1	\
                : __FILE__,                     \
                __FUNCTION__, __LINE__,         \
                ##__VA_ARGS__);                 \
        fflush(stderr);                         \
               } while(0)

#define ddebug(a) do {				\
        debug(#a " is %d", (a));    \
    } while(0)

#define xdebug(a) do {                      \
        debug(#a " is 0x%x", (a));          \
    } while(0)

#define die(fmt, ...) do {			\
        debug(fmt, ##__VA_ARGS__);  \
        exit(-1);                   \
    } while(0)

#define ASSERT_EQUAL(a,b) do {          \
        if ((a) != (b)) {                                   \
            printf("error: %s is not equal to %s\n ", #a,  #b); \
            exit(-1);                                           \
        } else {                                                \
            printf("OK: %s is equal to %s\n", #a, #b);          \
        }                                                       \
    } while (0)


#define ASSERT_NOT_EQUAL(a,b) do {      \
        if ((a) == (b)) {                               \
            printf("error: %s is equal to %s\n", #a, #b);	\
            exit(-1);                                       \
        } else {                                            \
            printf("OK: %s is not equal to %s\n", #a, #b);	\
        }                                                   \
    } while (0)

int main(int argc, char *argv[])
{

    int sample_rate=44100, channels=2, bits_per_sample=16;
    char *wav_file=NULL;
    prog_name = strdup(argv[0]);

    for (int i=1; i<argc; i++) {
        if (!strcasecmp(argv[i], "-s")) {
            check_1_more_arg(++i, argc);
            sample_rate = atoi(argv[i]);
        } else if (!strcasecmp(argv[i], "-c")) {
            check_1_more_arg(++i, argc);
            channels = atoi(argv[i]);
        } else if (!strcasecmp(argv[i], "-b")) {
            check_1_more_arg(++i, argc);
            bits_per_sample = atoi(argv[i]);
        } else {
            wav_file = argv[i];
        }
    }
    
    if (!wav_file)
        Usage();

    debug("%s", wav_file);
    ddebug(sample_rate);
    ddebug(channels);
    ddebug(bits_per_sample);

    open_dsp(sample_rate, channels, bits_per_sample);

    FILE* fp = fopen(wav_file, "r");
    if (!fp) {
        printf("error opening %s\n", wav_file, strerror(errno));
    }

    char buff[4096];
    int n;
    while(n=fread(buff, 1, 4096, fp)) {
        write(dsp_fd, buff, n);
    }
    fclose(fp);
    close(dsp_fd);
}
