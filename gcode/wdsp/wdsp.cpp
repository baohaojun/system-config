#include "bhjdebug.h"
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

    simple_debug(wav_file, %s);
    simple_debug(sample_rate, %d);
    simple_debug(channels, %d);
    simple_debug(bits_per_sample, %d);

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
