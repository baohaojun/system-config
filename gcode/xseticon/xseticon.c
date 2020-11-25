/*
 * Copyright (C) 2012, Paul Evans <leonerd@leonerd.org.uk>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include <glib.h>

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/Xmu/WinUtil.h>

#include <gd.h>

/* We can't use the one defined in Xmd.h because that's an "unsigned int",
 * which comes out as a 32bit type always. We need this to be 64bit on 64bit
 * machines.
 */
typedef unsigned long int CARD32;

char program_name[] = "xseticon";

gboolean verbose = FALSE;

void usage(int exitcode)
{
  printf("usage: %s [options] path/to/icon.png\n", program_name);
  printf("options:\n");
  printf("  -name <text>    : apply icon to the window of the name supplied\n");
  printf("  -id <windowid>  : apply icon to the window id supplied\n");
  printf("\n");
  printf("Sets the window icon to the specified .png image. The image is loaded from\n");
  printf("the file at runtime and sent to the X server; thereafter the file does not\n");
  printf("need to exist, and can be deleted/renamed/modified without the X server or\n");
  printf("window manager noticing.\n");
  printf("If no window selection option is specified, the window can be interactively\n");
  printf("selected using the cursor.\n");
  printf("\n");
  printf("Hints:\n");
  printf("  %s -id \"$WINDOWID\" path/to/icon.png\n", program_name);
  printf("Will set the icon for an xterm.\n");
  exit(exitcode);
}

// FROM programs/xlsfonts/dsimple.c

void Fatal_Error(char *msg, ...)
{
  va_list args;
  fflush(stdout);
  fflush(stderr);
  fprintf(stderr, "%s: error: ", program_name);
  va_start(args, msg);
  vfprintf(stderr, msg, args);
  va_end(args);
  fprintf(stderr, "\n");
  exit(1);
}

Window Window_With_Name(Display* dpy, Window top, char* name)
{
  Window *children, dummy;
  unsigned int nchildren;
  int i;
  Window w = 0;
  char *window_name;

  if (XFetchName(dpy, top, &window_name) && !strcmp(window_name, name))
    return(top);

  if (!XQueryTree(dpy, top, &dummy, &dummy, &children, &nchildren))
    return(0);

  for (i=0; i<nchildren; i++) {
          w = Window_With_Name(dpy, children[i], name);
          if (w)
            break;
  }
  if (children) XFree ((char *)children);
  return(w);
}

Window Select_Window_Args(Display* dpy, int screen, int* rargc, char* argv[])
{
  int nargc = 1;
  int argc;
  char **nargv;
  Window w = 0;

#define ARGC (*rargc)
  nargv = argv+1; argc = ARGC;
#define OPTION argv[0]
#define NXTOPTP ++argv, --argc>0
#define NXTOPT if (++argv, --argc==0) usage(1)
#define COPYOPT nargv++[0]=OPTION, nargc++

  while (NXTOPTP) {
    if (!strcmp(OPTION, "-")) {
      COPYOPT;
      while (NXTOPTP)
        COPYOPT;
      break;
    }
    if (!strcmp(OPTION, "-name")) {
      NXTOPT;
      if (verbose)
        printf("Selecting window by name %s\n", OPTION);
      w = Window_With_Name(dpy, RootWindow(dpy, screen),
                           OPTION);
      if (!w)
        Fatal_Error("No window with name %s exists!",OPTION);
      continue;
    }
    if (!strcmp(OPTION, "-id")) {
      NXTOPT;
      if (verbose)
        printf("Selecting window by ID %s\n", OPTION);
      w=0;
      sscanf(OPTION, "0x%lx", &w);
      if (!w)
        sscanf(OPTION, "%ld", &w);
      if (!w)
        Fatal_Error("Invalid window id format: %s.", OPTION);
      continue;
    }
    COPYOPT;
  }
  ARGC = nargc;
  
  return(w);
}

Window Select_Window_Mouse(Display* dpy, int screen)
{
  int status;
  Cursor cursor;
  XEvent event;
  Window target_win = None;
  Window root = RootWindow(dpy,screen);
  int buttons = 0;

  /* Make the target cursor */
  cursor = XCreateFontCursor(dpy, XC_crosshair);

  /* Grab the pointer using target cursor, letting it room all over */
  status = XGrabPointer(dpy, root, False,
                        ButtonPressMask|ButtonReleaseMask, GrabModeSync,
                        GrabModeAsync, root, cursor, CurrentTime);
  if (status != GrabSuccess) Fatal_Error("Can't grab the mouse.");

  /* Let the user select a window... */
  while ((target_win == None) || (buttons != 0)) {
    /* allow one more event */
    XAllowEvents(dpy, SyncPointer, CurrentTime);
    XWindowEvent(dpy, root, ButtonPressMask|ButtonReleaseMask, &event);
    switch (event.type) {
    case ButtonPress:
      if (target_win == None) {
        target_win = event.xbutton.subwindow; /* window selected */
        if (target_win == None) target_win = root;
      }
      buttons++;
      break;
    case ButtonRelease:
      if (buttons > 0) /* there may have been some down before we started */
        buttons--;
       break;
    }
  } 

  XUngrabPointer(dpy, CurrentTime);      /* Done with pointer */

  return(target_win);
}


// END FROM

void abortprog(gchar* fname)
{
  fprintf(stderr, "Aborted at function %s\n", fname);
  exit(1);
}

/* Note:
 *  dispite the fact this routine specifically loads 32bit data, it needs to
 *  load it into an unsigned long int array, not a guint32 array. The
 *  XChangeProperty() call wants to see a native size array when format == 32,
 *  not necessarily a 32bit one.
 */

void load_icon(gchar* filename, int* ndata, CARD32** data)
{
  FILE* iconfile = fopen(filename, "r");

  if (!iconfile) {
    abortprog("fopen()");
  }

  gdImagePtr icon = gdImageCreateFromPng(iconfile);

  fclose(iconfile);

  int width, height;

  width = gdImageSX(icon);
  height = gdImageSY(icon);

  if (verbose)
    printf("Loaded a %dx%d icon\n", width, height);

  (*ndata) = (width * height) + 2;

  (*data) = g_new0(CARD32, (*ndata));

  int i = 0;
  (*data)[i++] = width;
  (*data)[i++] = height;

  int x, y;

  for(y = 0; y < height; y++) {
    for(x = 0; x < width; x++) {
      // data is RGBA
      // We'll do some horrible data-munging here
      guint8* cols = (guint8*)&((*data)[i++]);
      
      int pixcolour = gdImageGetPixel(icon, x, y);

      cols[0] = gdImageBlue(icon, pixcolour);
      cols[1] = gdImageGreen(icon, pixcolour);
      cols[2] = gdImageRed(icon, pixcolour);

      /* Alpha is more difficult */
      int alpha = 127 - gdImageAlpha(icon, pixcolour); // 0 to 127
      
      // Scale it up to 0 to 255; remembering that 2*127 should be max
      if (alpha == 127)
        alpha = 255;
      else
        alpha *= 2;
      
      cols[3] = alpha;
    }
  }

  gdImageDestroy(icon);
}

int main(int argc, char* argv[])
{
  if (argc < 2 ||
      !strcmp(argv[1], "-h") ||
      !strcmp(argv[1], "--help"))
    usage(0);

  if (!argv[1])
    usage(1);

  guint argindex = 1;
  if (!strcmp(argv[argindex], "-v")) {
    verbose = TRUE;
    argindex++;
  }

  Display* display = XOpenDisplay(NULL);

  if (!display)
    abortprog("XOpenDisplay");

  XSynchronize(display, TRUE);

  int screen = DefaultScreen(display);

  Window window = Select_Window_Args(display, screen, &(argc), argv);

  if (!window) {
    if (verbose)
      printf("Selecting window by mouse...\n");
    window = Select_Window_Mouse(display, screen);
    if (window != None) {
      Window root;
      int dummyi;
      unsigned int dummy;

      if (XGetGeometry (display, window, &root, &dummyi, &dummyi,
                        &dummy, &dummy, &dummy, &dummy)
          && window != root)
          window = XmuClientWindow (display, window);
    }
  }

  if (verbose)
    printf("Have selected window 0x%08x\n", window);

  Atom property = XInternAtom(display, "_NET_WM_ICON", 0);

  if (!property)
    abortprog("XInternAtom(property)");

  guint nelements;
  CARD32* data;

  load_icon(argv[argindex], &nelements, &data);

  int result = XChangeProperty(display, window, property, XA_CARDINAL, 32, PropModeReplace, 
      (gchar*)data, nelements);

  if(!result)
    abortprog("XChangeProperty");

  result = XFlush(display);

  if(!result)
    abortprog("XFlush");

  XCloseDisplay(display);
}

