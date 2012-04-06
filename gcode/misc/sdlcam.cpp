/*

  sdlcam.cpp
  SDL + Video4Linux example

  ben chang
  bcchang.com

  compile:

  g++ sdlcam.cpp -lSDL -lSDLmain -o sdlcam

  tested on suse 10.1 with logitech quickcam 4000
  uses Video4Linux 2 (V4L2)

  i made this example based on the capture.c example from the video4linux documentation
  with SDL help from the tutorials at lazyfooproductions.com

  it assumes a camera whose data is in YUV 4:2:0 format, and includes YUV->RGB conversion that is, i'm
  sure, far from correct but at least gives a recognizable image.
*/


//The headers
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include <getopt.h>             /* getopt_long() */

#include <fcntl.h>              /* low-level i/o */
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <sys/ioctl.h>

#include <asm/types.h>          /* for videodev2.h */

#include <linux/videodev2.h>

#define CLEAR(x) memset (&(x), 0, sizeof (x))

#include "SDL/SDL.h"
#include <string>

//The attributes of the screen
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;
const int SCREEN_BPP = 32;
const int CAM_WIDTH = 320;
const int CAM_HEIGHT = 240;

int quit=false;
//The surfaces that will be used
SDL_Surface *message = NULL;
SDL_Surface *background = NULL;
SDL_Surface *screen = NULL;
SDL_Surface *cam_surface = NULL;

char *mypixels;
char *dev_name = "/dev/video0";

int fd = -1;

typedef enum {
	IO_METHOD_READ,
	IO_METHOD_MMAP,
	IO_METHOD_USERPTR,
} io_method;

typedef struct  {
	void * start;
	size_t length;
} buffer;

buffer *        	buffers	= NULL;
static io_method	io		= IO_METHOD_MMAP;
static unsigned int     n_buffers       = 0;

static void errno_exit (const char *s)
{
	fprintf (stderr, "%s error %d, %s\n",s, errno, strerror (errno));
	exit (EXIT_FAILURE);
}

static int xioctl (int fd, int request, void *arg)
{
	int r;

	do r = ioctl (fd, request, arg);
	while (-1 == r && EINTR == errno);
	return r;
}

int clamp (double x)
{
	int r = x;      /* round to nearest */

	if (r < 0)         return 0;
	else if (r > 255)  return 255;
	else               return r;
}


void yuv420_rgb (unsigned char Y1, unsigned char Cb, unsigned char Cr, int *ER, int *EG, int *EB)
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


static void process_image(const void * p)
{
	int x,y;
	char *campixdata = (char *) p;
	int p1,p2,p3,p4,p5,p6;
	int r,g,b;
	unsigned char Y,Cr,Cb;

	int Cr_start = CAM_WIDTH * CAM_HEIGHT;
	int Cb_start = Cr_start + (CAM_WIDTH*CAM_HEIGHT/4);

	for (y=0;y<CAM_HEIGHT;y++)
	{
		for (x=0;x<CAM_WIDTH;x++)
		{
			p1=(y*CAM_WIDTH) + (x);
			p2=((y/2)*CAM_WIDTH/2) + (x/2) + Cr_start;
			p3=((y/2)*CAM_WIDTH/2) + (x/2) + Cb_start;


			p4=(y*CAM_WIDTH*4 * 4) + (x*4 * 2);

			Y=campixdata[p1];
			Cr=campixdata[p2];
			Cb=campixdata[p3];
			
			yuv420_rgb (Y,Cr,Cb,&r,&g,&b);
			mypixels[p4]=r;
			mypixels[p4+1]=g;		
			mypixels[p4+2]=b;

			mypixels[p4+3]=255;

			if (x>0)
			{
				p5=p4-4;
				p6=p5-4;
				
				mypixels[p5] = (mypixels[p4]+mypixels[p6])/2;
				mypixels[p5+1] = (mypixels[p4+1]+mypixels[p6+1])/2;
				mypixels[p5+2] = (mypixels[p4+2]+mypixels[p6+2])/2;
			}
		}
	}

}
static int read_frame (void)
{
	v4l2_buffer buf;
	unsigned int i;
	switch (io) {
	case IO_METHOD_READ:
		if (-1 == read (fd, buffers[0].start, buffers[0].length)) 
		{
			switch (errno) {
            case EAGAIN:
                return 0;

            case EIO:	/* Could ignore EIO, see spec. */

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

		if (-1 == xioctl (fd, VIDIOC_DQBUF, &buf)) 
		{
			switch (errno) 
			{
            case EAGAIN:
                printf ("!");
                return 0;

            case EIO:		/* Could ignore EIO, see spec. */

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

		if (-1 == xioctl (fd, VIDIOC_DQBUF, &buf)) 
		{
			switch (errno) 
			{
            case EAGAIN:
                return 0;

            case EIO:/* Could ignore EIO, see spec. */
				
            default:
                errno_exit ("VIDIOC_DQBUF");
			}
		}

		for (i = 0; i < n_buffers; ++i)
			if (buf.m.userptr == (unsigned long) buffers[i].start && buf.length == buffers[i].length)
				break;

		assert (i < n_buffers);
		process_image ((void *) buf.m.userptr);

		if (-1 == xioctl (fd, VIDIOC_QBUF, &buf))
			errno_exit ("VIDIOC_QBUF");

		break;
	}

	return 1;
}

//////////////////////////// Update Cam /////////////////
int update_cam ()
{
	fd_set fds;
	struct timeval tv;
	int r;
	FD_ZERO (&fds);
	FD_SET (fd, &fds);

	/* Timeout. */
	tv.tv_sec = 1;
	tv.tv_usec = 0;

	r = select (fd + 1, &fds, NULL, NULL, &tv);

	return read_frame();

}

static void stop_capturing (void)
{
	enum v4l2_buf_type type;

	switch (io) 
	{
    case IO_METHOD_READ:	/* Nothing to do. */
        break;

    case IO_METHOD_MMAP:
    case IO_METHOD_USERPTR:
        type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
        if (-1 == xioctl (fd, VIDIOC_STREAMOFF, &type))
			errno_exit ("VIDIOC_STREAMOFF");
        break;
	}
}

static void start_capturing (void)
{
	unsigned int i;
	enum v4l2_buf_type type;

	switch (io) 
	{
    case IO_METHOD_READ:	/* Nothing to do. */
        break;
	
    case IO_METHOD_MMAP:
        for (i = 0; i < n_buffers; ++i) 
        {
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
        for (i = 0; i < n_buffers; ++i) 
        {
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

static void	uninit_device (void)
{
	unsigned int i;

	switch (io) 
	{
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

static void init_read (unsigned int buffer_size)
{
	buffers = (buffer*) calloc (1, sizeof (*buffers));

	if (!buffers) 
	{
		fprintf (stderr, "Out of memory\n");
		exit (EXIT_FAILURE);
	}

	buffers[0].length = buffer_size;
	buffers[0].start = malloc (buffer_size);

	if (!buffers[0].start) 
	{
		fprintf (stderr, "Out of memory\n");
		exit (EXIT_FAILURE);
	}
}

static void init_mmap (void)
{
	struct v4l2_requestbuffers req;

	CLEAR (req);

	req.count               = 4;
	req.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	req.memory              = V4L2_MEMORY_MMAP;

	if (-1 == xioctl (fd, VIDIOC_REQBUFS, &req)) 
	{
		if (EINVAL == errno) 
		{
			fprintf (stderr, "%s does not support memory mapping\n", dev_name);
			exit (EXIT_FAILURE);
		} 
		else 
		{
			errno_exit ("VIDIOC_REQBUFS");
		}
	}

	if (req.count < 2) 
	{
		fprintf (stderr, "Insufficient buffer memory on %s\n",dev_name);
		exit (EXIT_FAILURE);
	}

	buffers = (buffer*) calloc (req.count, sizeof (*buffers));

	if (!buffers) 
	{
		fprintf (stderr, "Out of memory\n");
		exit (EXIT_FAILURE);
	}

	for (n_buffers = 0; n_buffers < req.count; ++n_buffers) 
	{
		struct v4l2_buffer buf;

		CLEAR (buf);

		buf.type        = V4L2_BUF_TYPE_VIDEO_CAPTURE;
		buf.memory      = V4L2_MEMORY_MMAP;
		buf.index       = n_buffers;

		if (-1 == xioctl (fd, VIDIOC_QUERYBUF, &buf))
			errno_exit ("VIDIOC_QUERYBUF");

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

static void init_userp (unsigned int buffer_size)
{
	struct v4l2_requestbuffers req;

	CLEAR (req);

	req.count               = 4;
	req.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	req.memory              = V4L2_MEMORY_USERPTR;

	if (-1 == xioctl (fd, VIDIOC_REQBUFS, &req)) 
	{
		if (EINVAL == errno) 
		{
			fprintf (stderr, "%s does not support user pointer i/o\n", dev_name);
			exit (EXIT_FAILURE);
		} 
		else 
		{
			errno_exit ("VIDIOC_REQBUFS");
		}
	}

	buffers = (buffer*) calloc (4, sizeof (*buffers));

	if (!buffers) 
	{
		fprintf (stderr, "Out of memory\n");
		exit (EXIT_FAILURE);
	}

	for (n_buffers = 0; n_buffers < 4; ++n_buffers) 
	{
		buffers[n_buffers].length = buffer_size;
		buffers[n_buffers].start = malloc (buffer_size);

		if (!buffers[n_buffers].start) 
		{
			fprintf (stderr, "Out of memory\n");
			exit (EXIT_FAILURE);
		}
	}
}


static void init_device (void)
{
	struct v4l2_capability cap;
	struct v4l2_cropcap cropcap;
	struct v4l2_crop crop;
	struct v4l2_format fmt;
	unsigned int min;

	if (!(cap.capabilities & V4L2_CAP_VIDEO_CAPTURE)) 
	{
		fprintf (stderr, "%s is no video capture device\n",dev_name);
		exit (EXIT_FAILURE);
	}

	switch (io) 
	{
    case IO_METHOD_READ:
        if (!(cap.capabilities & V4L2_CAP_READWRITE)) 
        {
            fprintf (stderr, "%s does not support read i/o\n",dev_name);
            exit (EXIT_FAILURE);
        }
        break;

    case IO_METHOD_MMAP:
    case IO_METHOD_USERPTR:
        if (!(cap.capabilities & V4L2_CAP_STREAMING)) 
        {
            fprintf (stderr, "%s does not support streaming i/o\n",dev_name);
            exit (EXIT_FAILURE);
        }
        break;
	}

		
	/* Select video input, video standard and tune here. */

	CLEAR (cropcap);

	cropcap.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;

	if (0 == xioctl (fd, VIDIOC_CROPCAP, &cropcap)) 
	{
		crop.type = V4L2_BUF_TYPE_VIDEO_CAPTURE;
		crop.c = cropcap.defrect; /* reset to default */

		if (-1 == xioctl (fd, VIDIOC_S_CROP, &crop)) 
		{
			switch (errno) 
			{
            case EINVAL:
                /* Cropping not supported. */
                break;
            default:
                /* Errors ignored. */
                break;
			}
		}
	} 
	else 
	{	
		/* Errors ignored. */
	}

	CLEAR (fmt);

	fmt.type                = V4L2_BUF_TYPE_VIDEO_CAPTURE;
	fmt.fmt.pix.width       = CAM_WIDTH; 
	fmt.fmt.pix.height      = CAM_HEIGHT;
	//fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUYV;
	fmt.fmt.pix.pixelformat = V4L2_PIX_FMT_YUV420;
	//fmt.fmt.pix.field       = V4L2_FIELD_INTERLACED;

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

	switch (io) 
	{
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


static void open_device (void)
{
	fd = open (dev_name, O_RDWR | O_NONBLOCK, 0);

	if (-1 == fd) 
	{
		fprintf (stderr, "Cannot open %s: %d, %s\n",dev_name,errno, strerror (errno));
		exit (EXIT_FAILURE);
	}
}
static void close_device (void)
{
	if (-1 == close (fd))
		errno_exit ("close");
	fd = -1;
}



/**************** SDL image code ***************/

void createCamImage (int w,int h)
{
	int bpp,pitch;
	Uint32 rmask, gmask, bmask, amask;
	int x,y;
	int b;

	bpp=32;
	pitch = w * 4;

	amask = 0xff000000;
	bmask = 0x00ff0000;
	gmask = 0x0000ff00;
	rmask = 0x000000ff;
	mypixels = (char*)malloc (w*h*4);
	
	for (b=0;b<w*h*4;b++)
		mypixels[b]=255;	

	cam_surface = SDL_CreateRGBSurfaceFrom (mypixels,w,h,bpp,pitch,rmask,gmask,bmask,amask);
	for (y=0;y<h;y++)
	{
		for (x=0;x<w;x++)
		{
			// use this to make gradients
			/* 
               mypixels[y*pitch+x*4]=x%256;
               mypixels[y*pitch+x*4+1]=y%256;
               mypixels[y*pitch+x*4+2]=0;
               mypixels[y*pitch+x*4+3]=128;
			*/
			mypixels[y*pitch+x*4]=0;
			mypixels[y*pitch+x*4+1]=0;
			mypixels[y*pitch+x*4+2]=0;
			mypixels[y*pitch+x*4+3]=255;
			
		}
	}
}

SDL_Surface *load_image( std::string filename ) 
{
	//Temporary storage for the image that's loaded
	SDL_Surface* loadedImage = NULL;
	
	//The optimized image that will be used
	SDL_Surface* optimizedImage = NULL;
	
	//Load the image
	loadedImage = SDL_LoadBMP( filename.c_str() );
	
	//If nothing went wrong in loading the image
	if( loadedImage != NULL )
	{
		//Create an optimized image
		optimizedImage = SDL_DisplayFormat( loadedImage );
		
		//Free the old image
		SDL_FreeSurface( loadedImage );
	}
	
	//Return the optimized image
	return optimizedImage;
}

void apply_surface( int x, int y, SDL_Surface* source, SDL_Surface* destination )
{
	//Make a temporary rectangle to hold the offsets
	SDL_Rect offset;
	
	//Give the offsets to the rectangle
	offset.x = x;
	offset.y = y;

	//Blit the surface
	SDL_BlitSurface( source, NULL, destination, &offset );
}

int main( int argc, char* args[] )
{
	SDL_Event event;
	//Initialize all SDL subsystems
	if( SDL_Init( SDL_INIT_EVERYTHING ) == -1 )
	{
		return 1;    
	}

	//Set up the screen
	screen = SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, SDL_SWSURFACE );

	//If there was in error in setting up the screen
	if( screen == NULL )
	{
		return 1;    
	}

	//Set the window caption
	SDL_WM_SetCaption( "Video4Linux + SDL", NULL );

	//Load the images

	createCamImage (CAM_WIDTH*2,CAM_HEIGHT*2);

	apply_surface (0,0,cam_surface,screen);
	//Update the screen
	if( SDL_Flip( screen ) == -1 )
	{
		return 1;    
	}

	open_device ();
	init_device();

	while(!quit)
	{
		while(SDL_PollEvent(&event))
		{
			if (event.type == SDL_QUIT)
				quit=true;
			
		}
		if (update_cam ())
		{
			apply_surface (0,0,cam_surface,screen);
			if( SDL_Flip( screen ) == -1 ) { return 1; }
		}
	}


	//Free the surfaces

	SDL_FreeSurface (cam_surface);
	free(mypixels);
	//Quit SDL
	uninit_device();
	close_device();
	SDL_Quit();

	return 0;    
}
