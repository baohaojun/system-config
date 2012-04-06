/*
 *  V4L2 video capture example
 *
 *  This program can be used and distributed without restrictions.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <getopt.h>             /* getopt_long() */

#include <fcntl.h>              /* low-level i/o */
#include <unistd.h>
#include <errno.h>
#include <malloc.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <sys/ioctl.h>

#include <asm/types.h>          /* for videodev2.h */

#include <linux/videodev2.h>
#include <linux/fb.h>
#include <stdint.h>

#define CLEAR(x) memset (&(x), 0, sizeof (x))

#define debug(fmt, ...) do {			\
	fprintf(stderr,				\
		"%s %s() %d: " fmt "\n",	\
		strchr(__FILE__, '/')		\
		? strchr(__FILE__, '/') + 1	\
		: __FILE__,			\
		__FUNCTION__, __LINE__,		\
		##__VA_ARGS__);			\
	fflush(stderr);				\
    } while(0)

#define ddebug(a) do {				\
	debug(#a " is %d", (a));		\
    } while(0)

#define xdebug(a) do {				\
	debug(#a " is 0x%x", (a));		\
    } while(0)

#define die(fmt, ...) do {			\
	debug(fmt, ##__VA_ARGS__);		\
	exit(-1);				\
    } while(0)

#define ASSERT_EQUAL(a,b) do {					\
	if ((a) != (b)) {					\
	    printf("error: %s is not equal to %s\n ", #a,  #b); \
	    exit(-1);						\
	} else {						\
	    printf("OK: %s is equal to %s\n", #a, #b);		\
	}							\
    } while (0)


#define ASSERT_NOT_EQUAL(a,b) do {				\
	if ((a) == (b)) {					\
	    printf("error: %s is equal to %s\n", #a, #b);	\
	    exit(-1);						\
	} else {						\
	    printf("OK: %s is not equal to %s\n", #a, #b);	\
	}							\
    } while (0)

typedef enum {
    IO_METHOD_READ,
    IO_METHOD_MMAP,
    IO_METHOD_USERPTR,
} io_method;

struct buffer {
    void *                  start;
    size_t                  length;
};

static char *           dev_name        = NULL;
static io_method	io		= IO_METHOD_MMAP;
static int              fd              = -1;
struct buffer *         buffers         = NULL;
static unsigned int     n_buffers       = 0;

int fb_fd = -1;
struct fb_var_screeninfo var_info;
struct fb_fix_screeninfo fix_info;
unsigned char* fb_buffer;

static void
errno_exit                      (const char *           s)
{
    fprintf (stderr, "%s error %d, %s\n",
	     s, errno, strerror (errno));

    exit (EXIT_FAILURE);
}

static int
xioctl(int                    fd,
       int                    request,
       void *                 arg)
{
    int r;

    do r = ioctl (fd, request, arg);
    while (-1 == r && EINTR == errno);

    return r;
}

typedef struct {
    union {
	unsigned short int stype;                 /* Magic identifier            */
	unsigned char ctype[2];
    } type;
    unsigned int size;                       /* File size in bytes          */
    unsigned short int reserved1, reserved2;
    unsigned int offset;                     /* Offset to image data, bytes */
} __attribute__((packed)) bmp_file_header;

typedef struct {
    unsigned int size;               /* Header size in bytes      */
    int width,height;                /* Width and height of image */
    unsigned short int planes;       /* Number of colour planes   */
    unsigned short int bits;         /* Bits per pixel            */
    unsigned int compression;        /* Compression type          */
    unsigned int imagesize;          /* Image size in bytes       */
    int xresolution,yresolution;     /* Pixels per meter          */
    unsigned int ncolours;           /* Number of colours         */
    unsigned int importantcolours;   /* Important colours         */
} __attribute__((packed)) bmp_info_header;

unsigned char clamp (double x)
{
	int r = x;      /* round to nearest */

	if (r < 0)         return 0;
	else if (r > 255)  return 255;
	else               return r;
}

void yuv420_rgb (unsigned char Y1, unsigned char Cb, unsigned char Cr, unsigned char *ER, unsigned char *EG, unsigned char *EB)
{
	double r, g, b;         /* temporaries */
	double y1, pb, pr;
	
	y1 = (255 / 219.0) * (Y1 - 16);
	pb = (255 / 224.0) * (Cb - 128);
	pr = (255 / 224.0) * (Cr - 128);
	
	r = 1.0 * y1 + 0     * pb + 1.402 * pr;
	g = 1.0 * y1 - 0.344 * pb - 0.714 * pr;
	b = 1.0 * y1 + 1.772 * pb + 0     * pr;
	
	*ER = clamp (r); /* [ok? one should prob. limit y1,pb,pr] */
	*EG = clamp (g );
	*EB = clamp (b );
}

static void
process_image                   (const void *           p)
{

    int xres = 640;
    int yres = 480;

    unsigned char* yuvp = (unsigned char*) p;
    
    int x;
    int y;

    int xmin = xres < var_info.xres ? xres : var_info.xres;
    int ymin = yres < var_info.yres ? yres : var_info.yres;
    int fb_bytes_per_pix = var_info.bits_per_pixel/8;
    unsigned char* fb_start = fb_buffer + 
	fix_info.line_length * var_info.yoffset +
	var_info.xoffset * fb_bytes_per_pix;

    for (y = 0; y < ymin; y++) {
	unsigned char * fb_ystart = fb_start + fix_info.line_length * y;
	for (x = 0; x < xmin; x += 2) {
	    unsigned char* Y1p = yuvp + (2 * xres * y) + x;
	    unsigned char* Cbp = Y1p + 1;
	    unsigned char* Y2p = Y1p + 2;
	    unsigned char* Crp = Y1p + 3;
	    
	    unsigned char red1;
	    unsigned char green1;
	    unsigned char blue1;
	    
	    unsigned char red2;
	    unsigned char green2;
	    unsigned char blue2;

	    yuv420_rgb(*Y1p, *Cbp, *Crp, &red1, &green1, &blue1);
	    yuv420_rgb(*Y2p, *Cbp, *Crp, &red2, &green2, &blue2);

	    uint32_t data;

    /* start code-generator
       for y in 1 2; do
           cat << EOF
	   data = 
EOF
           for x in blue green red; do
           cat << EOF
(($x$y >> (8 - var_info.$x.length)) << var_info.$x.offset) |
EOF
           done
cat <<EOF
		0;
	    if (var_info.bits_per_pixel == 32) {
		*(uint32_t *) fb_ystart = data;
	    } else {
		*(uint16_t *) fb_ystart = data;
	    }

	    fb_ystart += fb_bytes_per_pix;
EOF
       done
       end code-generator */
    // start generated code
	    data = 
		((blue1 >> (8 - var_info.blue.length)) << var_info.blue.offset) |
		((green1 >> (8 - var_info.green.length)) << var_info.green.offset) |
		((red1 >> (8 - var_info.red.length)) << var_info.red.offset) |
		0;
	    if (var_info.bits_per_pixel == 32) {
		*(uint32_t *) fb_ystart = data;
	    } else {
		*(uint16_t *) fb_ystart = data;
	    }

	    fb_ystart += fb_bytes_per_pix;
	    data = 
		((blue2 >> (8 - var_info.blue.length)) << var_info.blue.offset) |
		((green2 >> (8 - var_info.green.length)) << var_info.green.offset) |
		((red2 >> (8 - var_info.red.length)) << var_info.red.offset) |
		0;
	    if (var_info.bits_per_pixel == 32) {
		*(uint32_t *) fb_ystart = data;
	    } else {
		*(uint16_t *) fb_ystart = data;
	    }

	    fb_ystart += fb_bytes_per_pix;

    // end generated code
	}
    }


    fputc ('.', stdout);
    fflush (stdout);
}


static int
read_frame(void)
{
    struct v4l2_buffer buf;
    unsigned int i;
    int ret;
    switch (io) {
    case IO_METHOD_READ:
	ret = read(fd, buffers[0].start, buffers[0].length);
	ddebug(ret);
	if (-1 == ret) {
	    switch (errno) {
	    case EAGAIN:
		return 0;

	    case EIO:
		/* Could ignore EIO, see spec. */

		/* fall through */

	    default:
		errno_exit ("read");
	    }
	}

	process_image (buffers[0].start);

	break;

    case IO_METHOD_MMAP:
	CLEAR (buf);

	buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	buf.memory = V4L2_MEMORY_MMAP;

	if (-1 == xioctl (fd, VIDIOC_DQBUF, &buf)) {
	    switch (errno) {
	    case EAGAIN:
		return 0;

	    case EIO:
		/* Could ignore EIO, see spec. */

		/* fall through */

	    default:
		errno_exit ("VIDIOC_DQBUF");
	    }
	}

	assert (buf.index < n_buffers);

	process_image (buffers[buf.index].start);

	if (-1 == xioctl (fd, VIDIOC_QBUF, &buf))
	    errno_exit ("VIDIOC_QBUF");

	break;

    case IO_METHOD_USERPTR:
	CLEAR (buf);

	buf.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	buf.memory = V4L2_MEMORY_USERPTR;

	if (-1 == xioctl (fd, VIDIOC_DQBUF, &buf)) {
	    switch (errno) {
	    case EAGAIN:
		return 0;

	    case EIO:
		/* Could ignore EIO, see spec. */

		/* fall through */

	    default:
		errno_exit ("VIDIOC_DQBUF");
	    }
	}

	for (i = 0; i < n_buffers; ++i)
	    if (buf.m.userptr == (unsigned long) buffers[i].start
		&& buf.length == buffers[i].length)
		break;

	assert (i < n_buffers);

	process_image ((void *) buf.m.userptr);

	if (-1 == xioctl (fd, VIDIOC_QBUF, &buf))
	    errno_exit ("VIDIOC_QBUF");

	break;
    }

    return 1;
}

static void
mainloop(void)
{
    unsigned int count;

    count = 100;

    while (count > 0) {
	for (;;) {
	    fd_set fds;
	    struct timeval tv;
	    int r;

	    FD_ZERO (&fds);
	    FD_SET (fd, &fds);

	    /* Timeout. */
	    tv.tv_sec = 2;
	    tv.tv_usec = 0;

	    r = select (fd + 1, &fds, NULL, NULL, &tv);

	    if (-1 == r) {
		if (EINTR == errno)
		    continue;

		errno_exit ("select");
	    }

	    if (0 == r) {
		fprintf (stderr, "select timeout\n");
		exit (EXIT_FAILURE);
	    }

	    if (read_frame ())
		break;
	
	    /* EAGAIN - continue select loop. */
	}
    }
}

static void
stop_capturing(void)
{
    enum v4l2_buf_type type;

    switch (io) {
    case IO_METHOD_READ:
	/* Nothing to do. */
	break;

    case IO_METHOD_MMAP:
    case IO_METHOD_USERPTR:
	type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

	if (-1 == xioctl (fd, VIDIOC_STREAMOFF, &type))
	    errno_exit ("VIDIOC_STREAMOFF");

	break;
    }
}

static void
start_capturing                 (void)
{
    unsigned int i;
    enum v4l2_buf_type type;

    switch (io) {
    case IO_METHOD_READ:
	/* Nothing to do. */
	break;

    case IO_METHOD_MMAP:
	for (i = 0; i < n_buffers; ++i) {
	    struct v4l2_buffer buf;

	    CLEAR (buf);

	    buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	    buf.memory      = V4L2_MEMORY_MMAP;
	    buf.index       = i;

	    if (-1 == xioctl (fd, VIDIOC_QBUF, &buf))
		errno_exit ("VIDIOC_QBUF");
	}
		
	type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

	if (-1 == xioctl (fd, VIDIOC_STREAMON, &type))
	    errno_exit ("VIDIOC_STREAMON");

	break;

    case IO_METHOD_USERPTR:
	for (i = 0; i < n_buffers; ++i) {
	    struct v4l2_buffer buf;

	    CLEAR (buf);

	    buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	    buf.memory      = V4L2_MEMORY_USERPTR;
	    buf.index       = i;
	    buf.m.userptr	= (unsigned long) buffers[i].start;
	    buf.length      = buffers[i].length;

	    if (-1 == xioctl (fd, VIDIOC_QBUF, &buf))
		errno_exit ("VIDIOC_QBUF");
	}

	type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

	if (-1 == xioctl (fd, VIDIOC_STREAMON, &type))
	    errno_exit ("VIDIOC_STREAMON");

	break;
    }
}

static void
uninit_device(void)
{
    unsigned int i;

    switch (io) {
    case IO_METHOD_READ:
	free (buffers[0].start);
	break;

    case IO_METHOD_MMAP:
	for (i = 0; i < n_buffers; ++i)
	    if (-1 == munmap (buffers[i].start, buffers[i].length))
		errno_exit ("munmap");
	break;

    case IO_METHOD_USERPTR:
	for (i = 0; i < n_buffers; ++i)
	    free (buffers[i].start);
	break;
    }

    free (buffers);
}

static void
init_read(unsigned int buffer_size)
{
    buffers = calloc (1, sizeof (*buffers));

    if (!buffers) {
	fprintf (stderr, "Out of memory\n");
	exit (EXIT_FAILURE);
    }

    buffers[0].length = buffer_size;
    buffers[0].start = malloc (buffer_size);

    if (!buffers[0].start) {
	fprintf (stderr, "Out of memory\n");
	exit (EXIT_FAILURE);
    }
}

static void
init_mmap			(void)
{
    struct v4l2_requestbuffers req;

    CLEAR (req);

    req.count               = 4;
    req.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    req.memory              = V4L2_MEMORY_MMAP;

    if (-1 == xioctl (fd, VIDIOC_REQBUFS, &req)) {
	if (EINVAL == errno) {
	    fprintf (stderr, "%s does not support "
		     "memory mapping\n", dev_name);
	    exit (EXIT_FAILURE);
	} else {
	    errno_exit ("VIDIOC_REQBUFS");
	}
    }

    if (req.count < 2) {
	fprintf (stderr, "Insufficient buffer memory on %s\n",
		 dev_name);
	exit (EXIT_FAILURE);
    }

    buffers = calloc (req.count, sizeof (*buffers));

    if (!buffers) {
	fprintf (stderr, "Out of memory\n");
	exit (EXIT_FAILURE);
    }

    for (n_buffers = 0; n_buffers < req.count; ++n_buffers) {
	struct v4l2_buffer buf;

	CLEAR (buf);

	buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	buf.memory      = V4L2_MEMORY_MMAP;
	buf.index       = n_buffers;

	if (-1 == xioctl (fd, VIDIOC_QUERYBUF, &buf))
	    errno_exit ("VIDIOC_QUERYBUF");

	xdebug(buf.length);
	ddebug(buf.length);
	buffers[n_buffers].length = buf.length;
	buffers[n_buffers].start =
	    mmap (NULL /* start anywhere */,
		  buf.length,
		  PROT_READ | PROT_WRITE /* required */,
		  MAP_SHARED /* recommended */,
		  fd, buf.m.offset);

	if (MAP_FAILED == buffers[n_buffers].start)
	    errno_exit ("mmap");
    }
}

static void
init_userp			(unsigned int		buffer_size)
{
    struct v4l2_requestbuffers req;
    unsigned int page_size;

    page_size = getpagesize ();
    buffer_size = (buffer_size + page_size - 1) & ~(page_size - 1);

    CLEAR (req);

    req.count               = 4;
    req.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    req.memory              = V4L2_MEMORY_USERPTR;

    if (-1 == xioctl (fd, VIDIOC_REQBUFS, &req)) {
	if (EINVAL == errno) {
	    fprintf (stderr, "%s does not support "
		     "user pointer i/o\n", dev_name);
	    exit (EXIT_FAILURE);
	} else {
	    errno_exit ("VIDIOC_REQBUFS");
	}
    }

    buffers = calloc (4, sizeof (*buffers));

    if (!buffers) {
	fprintf (stderr, "Out of memory\n");
	exit (EXIT_FAILURE);
    }

    for (n_buffers = 0; n_buffers < 4; ++n_buffers) {
	buffers[n_buffers].length = buffer_size;
	buffers[n_buffers].start = memalign (/* boundary */ page_size,
					     buffer_size);

	if (!buffers[n_buffers].start) {
	    fprintf (stderr, "Out of memory\n");
	    exit (EXIT_FAILURE);
	}
    }
}

static void
init_device                     (void)
{
    struct v4l2_capability cap;
    struct v4l2_cropcap cropcap;
    struct v4l2_crop crop;
    struct v4l2_format fmt;
    unsigned int min;

    if (-1 == xioctl (fd, VIDIOC_QUERYCAP, &cap)) {
	if (EINVAL == errno) {
	    fprintf (stderr, "%s is no V4L2 device\n",
		     dev_name);
	    exit (EXIT_FAILURE);
	} else {
	    errno_exit ("VIDIOC_QUERYCAP");
	}
    }

    xdebug(cap.capabilities);
    if (!(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE)) {
	fprintf (stderr, "%s is no video capture device\n",
		 dev_name);
	exit (EXIT_FAILURE);
    }

    switch (io) {
    case IO_METHOD_READ:
	if (!(cap.capabilities & V4L2_CAP_READWRITE)) {
	    fprintf (stderr, "%s does not support read i/o\n",
		     dev_name);
	    exit (EXIT_FAILURE);
	}

	break;

    case IO_METHOD_MMAP:
    case IO_METHOD_USERPTR:
	if (!(cap.capabilities & V4L2_CAP_STREAMING)) {
	    fprintf (stderr, "%s does not support streaming i/o\n",
		     dev_name);
	    exit (EXIT_FAILURE);
	}

	break;
    }


    /* Select video input, video standard and tune here. */


    CLEAR (cropcap);

    cropcap.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

    if (0 == xioctl (fd, VIDIOC_CROPCAP, &cropcap)) {
	crop.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	crop.c = cropcap.defrect; /* reset to default */

	if (-1 == xioctl (fd, VIDIOC_S_CROP, &crop)) {
	    switch (errno) {
	    case EINVAL:
		/* Cropping not supported. */
		break;
	    default:
		/* Errors ignored. */
		break;
	    }
	}
    } else {	
	/* Errors ignored. */
    }


    CLEAR (fmt);

    fmt.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
    fmt.fmt.pix.width       = 640; 
    fmt.fmt.pix.height      = 480;
    fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
    fmt.fmt.pix.field       = V4L2_FIELD_INTERLACED;

    if (-1 == xioctl (fd, VIDIOC_S_FMT, &fmt))
	errno_exit ("VIDIOC_S_FMT");

    /* Note VIDIOC_S_FMT may change width and height. */

    /* Buggy driver paranoia. */
    min = fmt.fmt.pix.width * 2;
    if (fmt.fmt.pix.bytesperline < min)
	fmt.fmt.pix.bytesperline = min;
    min = fmt.fmt.pix.bytesperline * fmt.fmt.pix.height;
    if (fmt.fmt.pix.sizeimage < min)
	fmt.fmt.pix.sizeimage = min;

    switch (io) {
    case IO_METHOD_READ:
	init_read (fmt.fmt.pix.sizeimage);
	break;

    case IO_METHOD_MMAP:
	init_mmap ();
	break;

    case IO_METHOD_USERPTR:
	init_userp (fmt.fmt.pix.sizeimage);
	break;
    }
}

static void
close_device(void)
{
    if (-1 == close (fd))
	errno_exit ("close");

    fd = -1;
}

static void
open_device                     (void)
{
    struct stat st; 

    if (-1 == stat (dev_name, &st)) {
	fprintf (stderr, "Cannot identify '%s': %d, %s\n",
		 dev_name, errno, strerror (errno));
	exit (EXIT_FAILURE);
    }

    if (!S_ISCHR (st.st_mode)) {
	fprintf (stderr, "%s is no device\n", dev_name);
	exit (EXIT_FAILURE);
    }

    fd = open (dev_name, O_RDWR /* required */ | O_NONBLOCK, 0);

    if (-1 == fd) {
	fprintf (stderr, "Cannot open '%s': %d, %s\n",
		 dev_name, errno, strerror (errno));
	exit (EXIT_FAILURE);
    }
}

static void
usage                           (FILE *                 fp,
				 int                    argc,
				 char **                argv)
{
    fprintf (fp,
	     "Usage: %s [options]\n\n"
	     "Options:\n"
	     "-d | --device name   Video device name [/dev/video]\n"
	     "-h | --help          Print this message\n"
	     "-m | --mmap          Use memory mapped buffers\n"
	     "-r | --read          Use read() calls\n"
	     "-u | --userp         Use application allocated buffers\n"
	     "",
	     argv[0]);
}

static const char short_options [] = "d:hmru";

static const struct option
long_options [] = {
    { "device",     required_argument,      NULL,           'd' },
    { "help",       no_argument,            NULL,           'h' },
    { "mmap",       no_argument,            NULL,           'm' },
    { "read",       no_argument,            NULL,           'r' },
    { "userp",      no_argument,            NULL,           'u' },
    { 0, 0, 0, 0 }
};

static void print_memory(void * mem, int size)
{
    int i;
    for (i=0; i < size; i++) {
	unsigned char *p = (unsigned char *)mem;
	printf ("%02x ", p[i]);
	if (i%16 == 15) {
	    printf ("\n");
	} else if (i%8 == 7) {
	    printf (" ");
	}
    }
    printf("\n\n");
}

static void
open_fb()
{
    int fb_fd = open("/dev/graphics/fb0", O_RDWR, 0);

    if (fb_fd < 0) { //try /dev/fb0
	fb_fd = open("/dev/fb0", O_RDWR, 0);
    }

    if (fb_fd < 0) {
	printf("error opening fb0\n");
	exit(-1);
    }
	
    if (ioctl(fb_fd, FBIOGET_VSCREENINFO, &var_info) < 0) {
	printf("error ioctl fbioget_vscreeninfo\n");
	exit(-1);
    }

    if (ioctl(fb_fd, FBIOGET_FSCREENINFO, &fix_info) < 0) {
	printf("error ioctl fbioget_fscreeninfo\n");
	exit(-1);
    }

    print_memory(&var_info, sizeof(var_info));
    print_memory(&fix_info, sizeof(fix_info));

/* struct fb_fix_screeninfo { */
/* 	char id[16];			/\* identification string eg "TT Builtin" *\/ */

    debug("id is %s", fix_info.id);
/* 	unsigned long smem_start;	/\* Start of frame buffer mem *\/ */
    xdebug(fix_info.smem_start);
/* 					/\* (physical address) *\/ */
/* 	__u32 smem_len;			/\* Length of frame buffer mem *\/ */
    xdebug(fix_info.smem_len);
/* 	__u32 type;			/\* see FB_TYPE_*		*\/ */
    xdebug(fix_info.type);
/* 	__u32 type_aux;			/\* Interleave for interleaved Planes *\/ */
    xdebug(fix_info.type_aux);
/* 	__u32 visual;			/\* see FB_VISUAL_*		*\/  */
    xdebug(fix_info.visual);
/* 	__u16 xpanstep;			/\* zero if no hardware panning  *\/ */
    xdebug(fix_info.xpanstep);
/* 	__u16 ypanstep;			/\* zero if no hardware panning  *\/ */
    xdebug(fix_info.ypanstep);
/* 	__u16 ywrapstep;		/\* zero if no hardware ywrap    *\/ */
    xdebug(fix_info.ywrapstep);
/* 	__u32 line_length;		/\* length of a line in bytes    *\/ */
    xdebug(fix_info.line_length);
/* 	unsigned long mmio_start;	/\* Start of Memory Mapped I/O   *\/ */
    xdebug(fix_info.mmio_start);
/* 					/\* (physical address) *\/ */
/* 	__u32 mmio_len;			/\* Length of Memory Mapped I/O  *\/ */
    xdebug(fix_info.mmio_len);
/* 	__u32 accel;			/\* Indicate to driver which	*\/ */
    xdebug(fix_info.accel);
/* 					/\*  specific chip/card we have	*\/ */
/* 	__u16 reserved[3];		/\* Reserved for future compatibility *\/ */
/* } */
/* struct fb_var_screeninfo { */
/* 	__u32 xres;			/\* visible resolution		*\/ */
    ddebug(var_info.xres);
/* 	__u32 yres; */
    ddebug(var_info.yres);
/* 	__u32 xres_virtual;		/\* virtual resolution		*\/ */
    ddebug(var_info.xres_virtual);
/* 	__u32 yres_virtual; */
    ddebug(var_info.yres_virtual);
/* 	__u32 xoffset;			/\* offset from virtual to visible *\/ */
    ddebug(var_info.xoffset);
/* 	__u32 yoffset;			/\* resolution			*\/ */
    ddebug(var_info.yoffset);

/* 	__u32 bits_per_pixel;		/\* guess what			*\/ */
    ddebug(var_info.bits_per_pixel);
/* 	__u32 grayscale;		/\* != 0 Graylevels instead of colors *\/ */
    ddebug(var_info.grayscale);

/* 	struct fb_bitfield red;		/\* bitfield in fb mem if true color, *\/ */
/* 	struct fb_bitfield green;	/\* else only length is significant *\/ */
/* 	struct fb_bitfield blue; */
/* 	struct fb_bitfield transp;	/\* transparency			*\/	 */

/* 	__u32 nonstd;			/\* != 0 Non standard pixel format *\/ */
    xdebug(var_info.nonstd);

/* 	__u32 activate;			/\* see FB_ACTIVATE_*		*\/ */
    xdebug(var_info.activate);

/* 	__u32 height;			/\* height of picture in mm    *\/ */
    xdebug(var_info.height);
/* 	__u32 width;			/\* width of picture in mm     *\/ */
    xdebug(var_info.width);

/* 	__u32 accel_flags;		/\* (OBSOLETE) see fb_info.flags *\/ */
    xdebug(var_info.accel_flags);

/* 	/\* Timing: All values in pixclocks, except pixclock (of course) *\/ */
/* 	__u32 pixclock;			/\* pixel clock in ps (pico seconds) *\/ */
    xdebug(var_info.pixclock);
/* 	__u32 left_margin;		/\* time from sync to picture	*\/ */
    xdebug(var_info.left_margin);
/* 	__u32 right_margin;		/\* time from picture to sync	*\/ */
    xdebug(var_info.right_margin);
/* 	__u32 upper_margin;		/\* time from sync to picture	*\/ */
    xdebug(var_info.upper_margin);
/* 	__u32 lower_margin; */
    xdebug(var_info.lower_margin);
/* 	__u32 hsync_len;		/\* length of horizontal sync	*\/ */
    xdebug(var_info.hsync_len);
/* 	__u32 vsync_len;		/\* length of vertical sync	*\/ */
    xdebug(var_info.vsync_len);
/* 	__u32 sync;			/\* see FB_SYNC_*		*\/ */
    xdebug(var_info.sync);
/* 	__u32 vmode;			/\* see FB_VMODE_*		*\/ */
    xdebug(var_info.vmode);
/* 	__u32 rotate;			/\* angle we rotate counter clockwise *\/ */
    xdebug(var_info.rotate);
/* 	__u32 reserved[5];		/\* Reserved for future compatibility *\/ */
/* } */


    fb_buffer  = (void*) mmap(
				 0, fix_info.smem_len,
				 PROT_READ | PROT_WRITE,
				 MAP_SHARED,
				 fb_fd, 0);

    ASSERT_NOT_EQUAL(fb_buffer, MAP_FAILED);

}

int
main (int                    argc,
      char **                argv)
{
    dev_name = "/dev/video0";

    for (;;) {
	int index;
	int c;
        
	c = getopt_long (argc, argv,
			 short_options, long_options,
			 &index);

	if (-1 == c)
	    break;

	switch (c) {
	case 0: /* getopt_long() flag */
	    break;

	case 'd':
	    dev_name = optarg;
	    break;

	case 'h':
	    usage (stdout, argc, argv);
	    exit (EXIT_SUCCESS);

	case 'm':
	    io = IO_METHOD_MMAP;
	    break;

	case 'r':
	    io = IO_METHOD_READ;
	    break;

	case 'u':
	    io = IO_METHOD_USERPTR;
	    break;

	default:
	    usage (stderr, argc, argv);
	    exit (EXIT_FAILURE);
	}
    }

    open_device ();

    init_device ();

    start_capturing ();

    open_fb();
    mainloop ();

    stop_capturing ();

    uninit_device ();

    close_device ();

    exit (EXIT_SUCCESS);

    return 0;
}


