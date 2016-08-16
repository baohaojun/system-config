/*
 *  Copyright (C) 2000-2002 Constantin Kaplinsky.  All Rights Reserved.
 *  Copyright (C) 2000 Tridia Corporation.  All Rights Reserved.
 *  Copyright (C) 1999 AT&T Laboratories Cambridge.  All Rights Reserved.
 *
 *  This is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
 *  USA.
 */

/*
 * rfbproto.c - functions to deal with client side of RFB protocol.
 */

#ifdef __STRICT_ANSI__
#define _BSD_SOURCE
#define _POSIX_SOURCE
#define _XOPEN_SOURCE 600
#endif
#ifndef WIN32
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#endif
#include <errno.h>
#include <rfb/rfbclient.h>
#ifdef WIN32
#undef SOCKET
#undef socklen_t
#endif
#ifdef LIBVNCSERVER_HAVE_LIBZ
#include <zlib.h>
#ifdef __CHECKER__
#undef Z_NULL
#define Z_NULL NULL
#endif
#endif
#ifdef LIBVNCSERVER_HAVE_LIBJPEG
#ifdef _RPCNDR_H /* This Windows header typedefs 'boolean', jpeglib has to know */
#define HAVE_BOOLEAN
#endif
#include <jpeglib.h>
#endif

#ifndef _MSC_VER
/* Strings.h is not available in MSVC */
#include <strings.h>
#endif

#include <stdarg.h>
#include <time.h>

#ifdef LIBVNCSERVER_WITH_CLIENT_GCRYPT
#include <gcrypt.h>
#endif

#include "minilzo.h"
#include "tls.h"

#ifdef _MSC_VER
#  define snprintf _snprintf /* MSVC went straight to the underscored syntax */
#endif

/*
 * rfbClientLog prints a time-stamped message to the log file (stderr).
 */

rfbBool rfbEnableClientLogging=true;

static void rfbDefaultClientLog(const char *format, ...)
{
    va_list args;
    char buf[256];
    time_t log_clock;

    if(!rfbEnableClientLogging)
      return;

    va_start(args, format);

    time(&log_clock);
    strftime(buf, 255, "%d/%m/%Y %X ", localtime(&log_clock));
    fprintf(stderr, "%s", buf);

    vfprintf(stderr, format, args);
    fflush(stderr);

    va_end(args);
}

rfbClientLogProc rfbClientLog=rfbDefaultClientLog;
rfbClientLogProc rfbClientErr=rfbDefaultClientLog;

/* extensions */

rfbClientProtocolExtension* rfbClientExtensions = NULL;

void rfbClientRegisterExtension(rfbClientProtocolExtension* e)
{
	e->next = rfbClientExtensions;
	rfbClientExtensions = e;
}

/* client data */

void rfbClientSetClientData(rfbClient* client, void* tag, void* data)
{
	rfbClientData* clientData = client->clientData;

	while(clientData && clientData->tag != tag)
		clientData = clientData->next;
	if(clientData == NULL) {
		clientData = calloc(sizeof(rfbClientData), 1);
		clientData->next = client->clientData;
		client->clientData = clientData;
		clientData->tag = tag;
	}

	clientData->data = data;
}

void* rfbClientGetClientData(rfbClient* client, void* tag)
{
	rfbClientData* clientData = client->clientData;

	while(clientData) {
		if(clientData->tag == tag)
			return clientData->data;
		clientData = clientData->next;
	}

	return NULL;
}

/* messages */

static void FillRectangle(rfbClient* client, int x, int y, int w, int h, uint32_t colour) {
  int i,j;

#define FILL_RECT(BPP) \
    for(j=y*client->width;j<(y+h)*client->width;j+=client->width) \
      for(i=x;i<x+w;i++) \
	((uint##BPP##_t*)client->frameBuffer)[j+i]=colour;

  switch(client->format.bitsPerPixel) {
  case  8: FILL_RECT(8);  break;
  case 16: FILL_RECT(16); break;
  case 32: FILL_RECT(32); break;
  default:
    rfbClientLog("Unsupported bitsPerPixel: %d\n",client->format.bitsPerPixel);
  }
}

static void CopyRectangle(rfbClient* client, uint8_t* buffer, int x, int y, int w, int h) {
  int j;

  if (client->frameBuffer == NULL) {
      return;
  }

#define COPY_RECT(BPP) \
  { \
    int rs = w * BPP / 8, rs2 = client->width * BPP / 8; \
    for (j = ((x * (BPP / 8)) + (y * rs2)); j < (y + h) * rs2; j += rs2) { \
      memcpy(client->frameBuffer + j, buffer, rs); \
      buffer += rs; \
    } \
  }

  switch(client->format.bitsPerPixel) {
  case  8: COPY_RECT(8);  break;
  case 16: COPY_RECT(16); break;
  case 32: COPY_RECT(32); break;
  default:
    rfbClientLog("Unsupported bitsPerPixel: %d\n",client->format.bitsPerPixel);
  }
}

/* TODO: test */
static void CopyRectangleFromRectangle(rfbClient* client, int src_x, int src_y, int w, int h, int dest_x, int dest_y) {
  int i,j;

#define COPY_RECT_FROM_RECT(BPP) \
  { \
    uint##BPP##_t* _buffer=((uint##BPP##_t*)client->frameBuffer)+(src_y-dest_y)*client->width+src_x-dest_x; \
    if (dest_y < src_y) { \
      for(j = dest_y*client->width; j < (dest_y+h)*client->width; j += client->width) { \
        if (dest_x < src_x) { \
          for(i = dest_x; i < dest_x+w; i++) { \
            ((uint##BPP##_t*)client->frameBuffer)[j+i]=_buffer[j+i]; \
          } \
        } else { \
          for(i = dest_x+w-1; i >= dest_x; i--) { \
            ((uint##BPP##_t*)client->frameBuffer)[j+i]=_buffer[j+i]; \
          } \
        } \
      } \
    } else { \
      for(j = (dest_y+h-1)*client->width; j >= dest_y*client->width; j-=client->width) { \
        if (dest_x < src_x) { \
          for(i = dest_x; i < dest_x+w; i++) { \
            ((uint##BPP##_t*)client->frameBuffer)[j+i]=_buffer[j+i]; \
          } \
        } else { \
          for(i = dest_x+w-1; i >= dest_x; i--) { \
            ((uint##BPP##_t*)client->frameBuffer)[j+i]=_buffer[j+i]; \
          } \
        } \
      } \
    } \
  }

  switch(client->format.bitsPerPixel) {
  case  8: COPY_RECT_FROM_RECT(8);  break;
  case 16: COPY_RECT_FROM_RECT(16); break;
  case 32: COPY_RECT_FROM_RECT(32); break;
  default:
    rfbClientLog("Unsupported bitsPerPixel: %d\n",client->format.bitsPerPixel);
  }
}

static rfbBool HandleRRE8(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleRRE16(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleRRE32(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleCoRRE8(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleCoRRE16(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleCoRRE32(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleHextile8(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleHextile16(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleHextile32(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleUltra8(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleUltra16(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleUltra32(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleUltraZip8(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleUltraZip16(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleUltraZip32(rfbClient* client, int rx, int ry, int rw, int rh);
#ifdef LIBVNCSERVER_HAVE_LIBZ
static rfbBool HandleZlib8(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleZlib16(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleZlib32(rfbClient* client, int rx, int ry, int rw, int rh);
#ifdef LIBVNCSERVER_HAVE_LIBJPEG
static rfbBool HandleTight8(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleTight16(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleTight32(rfbClient* client, int rx, int ry, int rw, int rh);

static long ReadCompactLen (rfbClient* client);

static void JpegInitSource(j_decompress_ptr cinfo);
static boolean JpegFillInputBuffer(j_decompress_ptr cinfo);
static void JpegSkipInputData(j_decompress_ptr cinfo, long num_bytes);
static void JpegTermSource(j_decompress_ptr cinfo);
static void JpegSetSrcManager(j_decompress_ptr cinfo, uint8_t *compressedData,
                              int compressedLen);
#endif
static rfbBool HandleZRLE8(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleZRLE15(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleZRLE16(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleZRLE24(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleZRLE24Up(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleZRLE24Down(rfbClient* client, int rx, int ry, int rw, int rh);
static rfbBool HandleZRLE32(rfbClient* client, int rx, int ry, int rw, int rh);
#endif
#ifdef LIBVNCSERVER_CONFIG_LIBVA
static rfbBool HandleH264 (rfbClient* client, int rx, int ry, int rw, int rh);
#endif

/*
 * Server Capability Functions
 */
rfbBool SupportsClient2Server(rfbClient* client, int messageType)
{
    return (client->supportedMessages.client2server[((messageType & 0xFF)/8)] & (1<<(messageType % 8)) ? true : false);
}

rfbBool SupportsServer2Client(rfbClient* client, int messageType)
{
    return (client->supportedMessages.server2client[((messageType & 0xFF)/8)] & (1<<(messageType % 8)) ? true : false);
}

void SetClient2Server(rfbClient* client, int messageType)
{
  client->supportedMessages.client2server[((messageType & 0xFF)/8)] |= (1<<(messageType % 8));
}

void SetServer2Client(rfbClient* client, int messageType)
{
  client->supportedMessages.server2client[((messageType & 0xFF)/8)] |= (1<<(messageType % 8));
}

void ClearClient2Server(rfbClient* client, int messageType)
{
  client->supportedMessages.client2server[((messageType & 0xFF)/8)] &= (!(1<<(messageType % 8)));
}

void ClearServer2Client(rfbClient* client, int messageType)
{
  client->supportedMessages.server2client[((messageType & 0xFF)/8)] &= (!(1<<(messageType % 8)));
}

void DefaultSupportedMessages(rfbClient* client)
{
    memset((char *)&client->supportedMessages,0,sizeof(client->supportedMessages));

    /* Default client supported messages (universal RFB 3.3 protocol) */
    SetClient2Server(client, rfbSetPixelFormat);
    /* SetClient2Server(client, rfbFixColourMapEntries); Not currently supported */
    SetClient2Server(client, rfbSetEncodings);
    SetClient2Server(client, rfbFramebufferUpdateRequest);
    SetClient2Server(client, rfbKeyEvent);
    SetClient2Server(client, rfbPointerEvent);
    SetClient2Server(client, rfbClientCutText);
    /* technically, we only care what we can *send* to the server
     * but, we set Server2Client Just in case it ever becomes useful
     */
    SetServer2Client(client, rfbFramebufferUpdate);
    SetServer2Client(client, rfbSetColourMapEntries);
    SetServer2Client(client, rfbBell);
    SetServer2Client(client, rfbServerCutText);
}

void DefaultSupportedMessagesUltraVNC(rfbClient* client)
{
    DefaultSupportedMessages(client);
    SetClient2Server(client, rfbFileTransfer);
    SetClient2Server(client, rfbSetScale);
    SetClient2Server(client, rfbSetServerInput);
    SetClient2Server(client, rfbSetSW);
    SetClient2Server(client, rfbTextChat);
    SetClient2Server(client, rfbPalmVNCSetScaleFactor);
    /* technically, we only care what we can *send* to the server */
    SetServer2Client(client, rfbResizeFrameBuffer);
    SetServer2Client(client, rfbPalmVNCReSizeFrameBuffer);
    SetServer2Client(client, rfbFileTransfer);
    SetServer2Client(client, rfbTextChat);
}

void DefaultSupportedMessagesTightVNC(rfbClient* client)
{
    DefaultSupportedMessages(client);
    SetClient2Server(client, rfbFileTransfer);
    SetClient2Server(client, rfbSetServerInput);
    SetClient2Server(client, rfbSetSW);
    /* SetClient2Server(client, rfbTextChat); */
    /* technically, we only care what we can *send* to the server */
    SetServer2Client(client, rfbFileTransfer);
    SetServer2Client(client, rfbTextChat);
}

#ifndef WIN32
static rfbBool IsUnixSocket(const char *name)
{
  struct stat sb;
  if(stat(name, &sb) == 0 && (sb.st_mode & S_IFMT) == S_IFSOCK)
    return true;
  return false;
}
#endif

/*
 * ConnectToRFBServer.
 */

rfbBool ConnectToRFBServer(rfbClient* client,const char *hostname, int port)
{
  if (client->serverPort==-1) {
    /* serverHost is a file recorded by vncrec. */
    const char* magic="vncLog0.0";
    char buffer[10];
    rfbVNCRec* rec = (rfbVNCRec*)malloc(sizeof(rfbVNCRec));
    client->vncRec = rec;

    rec->file = fopen(client->serverHost,"rb");
    rec->tv.tv_sec = 0;
    rec->readTimestamp = false;
    rec->doNotSleep = false;
    
    if (!rec->file) {
      rfbClientLog("Could not open %s.\n",client->serverHost);
      return false;
    }
    setbuf(rec->file,NULL);

    if (fread(buffer,1,strlen(magic),rec->file) != strlen(magic) || strncmp(buffer,magic,strlen(magic))) {
      rfbClientLog("File %s was not recorded by vncrec.\n",client->serverHost);
      fclose(rec->file);
      return false;
    }
    client->sock = -1;
    return true;
  }

#ifndef WIN32
  if(IsUnixSocket(hostname))
    /* serverHost is a UNIX socket. */
    client->sock = ConnectClientToUnixSock(hostname);
  else
#endif
  {
#ifdef LIBVNCSERVER_IPv6
    client->sock = ConnectClientToTcpAddr6(hostname, port);
    if (client->sock == -1)
#endif
    {
      unsigned int host;

      /* serverHost is a hostname */
      if (!StringToIPAddr(hostname, &host)) {
        rfbClientLog("Couldn't convert '%s' to host address\n", hostname);
        return false;
      }
      client->sock = ConnectClientToTcpAddr(host, port);
    }
  }

  if (client->sock < 0) {
    rfbClientLog("Unable to connect to VNC server\n");
    return false;
  }

  if(client->QoS_DSCP && !SetDSCP(client->sock, client->QoS_DSCP))
     return false;

  return SetNonBlocking(client->sock);
}

/*
 * ConnectToRFBRepeater.
 */

rfbBool ConnectToRFBRepeater(rfbClient* client,const char *repeaterHost, int repeaterPort, const char *destHost, int destPort)
{
  rfbProtocolVersionMsg pv;
  int major,minor;
  char tmphost[250];

#ifdef LIBVNCSERVER_IPv6
  client->sock = ConnectClientToTcpAddr6(repeaterHost, repeaterPort);
  if (client->sock == -1)
#endif
  {
    unsigned int host;
    if (!StringToIPAddr(repeaterHost, &host)) {
      rfbClientLog("Couldn't convert '%s' to host address\n", repeaterHost);
      return false;
    }

    client->sock = ConnectClientToTcpAddr(host, repeaterPort);
  }

  if (client->sock < 0) {
    rfbClientLog("Unable to connect to VNC repeater\n");
    return false;
  }

  if (!SetNonBlocking(client->sock))
    return false;

  if (!ReadFromRFBServer(client, pv, sz_rfbProtocolVersionMsg))
    return false;
  pv[sz_rfbProtocolVersionMsg] = 0;

  /* UltraVNC repeater always report version 000.000 to identify itself */
  if (sscanf(pv,rfbProtocolVersionFormat,&major,&minor) != 2 || major != 0 || minor != 0) {
    rfbClientLog("Not a valid VNC repeater (%s)\n",pv);
    return false;
  }

  rfbClientLog("Connected to VNC repeater, using protocol version %d.%d\n", major, minor);

  snprintf(tmphost, sizeof(tmphost), "%s:%d", destHost, destPort);
  if (!WriteToRFBServer(client, tmphost, sizeof(tmphost)))
    return false;

  return true;
}

extern void rfbClientEncryptBytes(unsigned char* bytes, char* passwd);
extern void rfbClientEncryptBytes2(unsigned char *where, const int length, unsigned char *key);

rfbBool rfbHandleAuthResult(rfbClient* client)
{
    uint32_t authResult=0, reasonLen=0;
    char *reason=NULL;

    if (!ReadFromRFBServer(client, (char *)&authResult, 4)) return false;

    authResult = rfbClientSwap32IfLE(authResult);

    switch (authResult) {
    case rfbVncAuthOK:
      rfbClientLog("VNC authentication succeeded\n");
      return true;
      break;
    case rfbVncAuthFailed:
      if (client->major==3 && client->minor>7)
      {
        /* we have an error following */
        if (!ReadFromRFBServer(client, (char *)&reasonLen, 4)) return false;
        reasonLen = rfbClientSwap32IfLE(reasonLen);
        reason = malloc(reasonLen+1);
        if (!ReadFromRFBServer(client, reason, reasonLen)) { free(reason); return false; }
        reason[reasonLen]=0;
        rfbClientLog("VNC connection failed: %s\n",reason);
        free(reason);
        return false;
      }
      rfbClientLog("VNC authentication failed\n");
      return false;
    case rfbVncAuthTooMany:
      rfbClientLog("VNC authentication failed - too many tries\n");
      return false;
    }

    rfbClientLog("Unknown VNC authentication result: %d\n",
                 (int)authResult);
    return false;
}

static void ReadReason(rfbClient* client)
{
    uint32_t reasonLen;
    char *reason;

    /* we have an error following */
    if (!ReadFromRFBServer(client, (char *)&reasonLen, 4)) return;
    reasonLen = rfbClientSwap32IfLE(reasonLen);
    reason = malloc(reasonLen+1);
    if (!ReadFromRFBServer(client, reason, reasonLen)) { free(reason); return; }
    reason[reasonLen]=0;
    rfbClientLog("VNC connection failed: %s\n",reason);
    free(reason);
}

static rfbBool ReadSupportedSecurityType(rfbClient* client, uint32_t *result, rfbBool subAuth)
{
    uint8_t count=0;
    uint8_t loop=0;
    uint8_t flag=0;
    uint8_t tAuth[256];
    char buf1[500],buf2[10];
    uint32_t authScheme;

    if (!ReadFromRFBServer(client, (char *)&count, 1)) return false;

    if (count==0)
    {
        rfbClientLog("List of security types is ZERO, expecting an error to follow\n");
        ReadReason(client);
        return false;
    }

    rfbClientLog("We have %d security types to read\n", count);
    authScheme=0;
    /* now, we have a list of available security types to read ( uint8_t[] ) */
    for (loop=0;loop<count;loop++)
    {
        if (!ReadFromRFBServer(client, (char *)&tAuth[loop], 1)) return false;
        rfbClientLog("%d) Received security type %d\n", loop, tAuth[loop]);
        if (flag) continue;
        if (tAuth[loop]==rfbVncAuth || tAuth[loop]==rfbNoAuth ||
#if defined(LIBVNCSERVER_HAVE_GNUTLS) || defined(LIBVNCSERVER_HAVE_LIBSSL)
            tAuth[loop]==rfbVeNCrypt ||
#endif
            (tAuth[loop]==rfbARD && client->GetCredential) ||
            (!subAuth && (tAuth[loop]==rfbTLS || (tAuth[loop]==rfbVeNCrypt && client->GetCredential))))
        {
            if (!subAuth && client->clientAuthSchemes)
            {
                int i;
                for (i=0;client->clientAuthSchemes[i];i++)
                {
                    if (client->clientAuthSchemes[i]==(uint32_t)tAuth[loop])
                    {
                        flag++;
                        authScheme=tAuth[loop];
                        break;
                    }
                }
            }
            else
            {
                flag++;
                authScheme=tAuth[loop];
            }
            if (flag)
            {
                rfbClientLog("Selecting security type %d (%d/%d in the list)\n", authScheme, loop, count);
                /* send back a single byte indicating which security type to use */
                if (!WriteToRFBServer(client, (char *)&tAuth[loop], 1)) return false;
            }
        }
    }
    if (authScheme==0)
    {
        memset(buf1, 0, sizeof(buf1));
        for (loop=0;loop<count;loop++)
        {
            if (strlen(buf1)>=sizeof(buf1)-1) break;
            snprintf(buf2, sizeof(buf2), (loop>0 ? ", %d" : "%d"), (int)tAuth[loop]);
            strncat(buf1, buf2, sizeof(buf1)-strlen(buf1)-1);
        }
        rfbClientLog("Unknown authentication scheme from VNC server: %s\n",
               buf1);
        return false;
    }
    *result = authScheme;
    return true;
}

static rfbBool HandleVncAuth(rfbClient *client)
{
    uint8_t challenge[CHALLENGESIZE];
    char *passwd=NULL;
    int i;

    if (!ReadFromRFBServer(client, (char *)challenge, CHALLENGESIZE)) return false;

    if (client->serverPort!=-1) { /* if not playing a vncrec file */
      if (client->GetPassword)
        passwd = client->GetPassword(client);

      if ((!passwd) || (strlen(passwd) == 0)) {
        rfbClientLog("Reading password failed\n");
        return false;
      }
      if (strlen(passwd) > 8) {
        passwd[8] = '\0';
      }

      rfbClientEncryptBytes(challenge, passwd);

      /* Lose the password from memory */
      for (i = strlen(passwd); i >= 0; i--) {
        passwd[i] = '\0';
      }
      free(passwd);

      if (!WriteToRFBServer(client, (char *)challenge, CHALLENGESIZE)) return false;
    }

    /* Handle the SecurityResult message */
    if (!rfbHandleAuthResult(client)) return false;

    return true;
}

static void FreeUserCredential(rfbCredential *cred)
{
  if (cred->userCredential.username) free(cred->userCredential.username);
  if (cred->userCredential.password) free(cred->userCredential.password);
  free(cred);
}

static rfbBool HandlePlainAuth(rfbClient *client)
{
  uint32_t ulen, ulensw;
  uint32_t plen, plensw;
  rfbCredential *cred;

  if (!client->GetCredential)
  {
    rfbClientLog("GetCredential callback is not set.\n");
    return false;
  }
  cred = client->GetCredential(client, rfbCredentialTypeUser);
  if (!cred)
  {
    rfbClientLog("Reading credential failed\n");
    return false;
  }

  ulen = (cred->userCredential.username ? strlen(cred->userCredential.username) : 0);
  ulensw = rfbClientSwap32IfLE(ulen);
  plen = (cred->userCredential.password ? strlen(cred->userCredential.password) : 0);
  plensw = rfbClientSwap32IfLE(plen);
  if (!WriteToRFBServer(client, (char *)&ulensw, 4) ||
      !WriteToRFBServer(client, (char *)&plensw, 4))
  {
    FreeUserCredential(cred);
    return false;
  }
  if (ulen > 0)
  {
    if (!WriteToRFBServer(client, cred->userCredential.username, ulen))
    {
      FreeUserCredential(cred);
      return false;
    }
  }
  if (plen > 0)
  {
    if (!WriteToRFBServer(client, cred->userCredential.password, plen))
    {
      FreeUserCredential(cred);
      return false;
    }
  }

  FreeUserCredential(cred);

  /* Handle the SecurityResult message */
  if (!rfbHandleAuthResult(client)) return false;

  return true;
}

/* Simple 64bit big integer arithmetic implementation */
/* (x + y) % m, works even if (x + y) > 64bit */
#define rfbAddM64(x,y,m) ((x+y)%m+(x+y<x?(((uint64_t)-1)%m+1)%m:0))
/* (x * y) % m */
static uint64_t rfbMulM64(uint64_t x, uint64_t y, uint64_t m)
{
  uint64_t r;
  for(r=0;x>0;x>>=1)
  {
    if (x&1) r=rfbAddM64(r,y,m);
    y=rfbAddM64(y,y,m);
  }
  return r;
}

/* (x ^ y) % m */
static uint64_t rfbPowM64(uint64_t b, uint64_t e, uint64_t m)
{
  uint64_t r;
  for(r=1;e>0;e>>=1)
  {
    if(e&1) r=rfbMulM64(r,b,m);
    b=rfbMulM64(b,b,m);
  }
  return r;
}

static rfbBool HandleMSLogonAuth(rfbClient *client)
{
  uint64_t gen, mod, resp, priv, pub, key;
  uint8_t username[256], password[64];
  rfbCredential *cred;

  if (!ReadFromRFBServer(client, (char *)&gen, 8)) return false;
  if (!ReadFromRFBServer(client, (char *)&mod, 8)) return false;
  if (!ReadFromRFBServer(client, (char *)&resp, 8)) return false;
  gen = rfbClientSwap64IfLE(gen);
  mod = rfbClientSwap64IfLE(mod);
  resp = rfbClientSwap64IfLE(resp);

  if (!client->GetCredential)
  {
    rfbClientLog("GetCredential callback is not set.\n");
    return false;
  }
  rfbClientLog("WARNING! MSLogon security type has very low password encryption! "\
    "Use it only with SSH tunnel or trusted network.\n");
  cred = client->GetCredential(client, rfbCredentialTypeUser);
  if (!cred)
  {
    rfbClientLog("Reading credential failed\n");
    return false;
  }

  memset(username, 0, sizeof(username));
  strncpy((char *)username, cred->userCredential.username, sizeof(username));
  memset(password, 0, sizeof(password));
  strncpy((char *)password, cred->userCredential.password, sizeof(password));
  FreeUserCredential(cred);

  srand(time(NULL));
  priv = ((uint64_t)rand())<<32;
  priv |= (uint64_t)rand();

  pub = rfbPowM64(gen, priv, mod);
  key = rfbPowM64(resp, priv, mod);
  pub = rfbClientSwap64IfLE(pub);
  key = rfbClientSwap64IfLE(key);

  rfbClientEncryptBytes2(username, sizeof(username), (unsigned char *)&key);
  rfbClientEncryptBytes2(password, sizeof(password), (unsigned char *)&key);

  if (!WriteToRFBServer(client, (char *)&pub, 8)) return false;
  if (!WriteToRFBServer(client, (char *)username, sizeof(username))) return false;
  if (!WriteToRFBServer(client, (char *)password, sizeof(password))) return false;

  /* Handle the SecurityResult message */
  if (!rfbHandleAuthResult(client)) return false;

  return true;
}

#ifdef LIBVNCSERVER_WITH_CLIENT_GCRYPT
static rfbBool rfbMpiToBytes(const gcry_mpi_t value, uint8_t *result, size_t size)
{
  gcry_error_t error;
  size_t len;
  int i;

  error = gcry_mpi_print(GCRYMPI_FMT_USG, result, size, &len, value);
  if (gcry_err_code(error) != GPG_ERR_NO_ERROR)
  {
    rfbClientLog("gcry_mpi_print error: %s\n", gcry_strerror(error));
    return false;
  }
  for (i=size-1;i>(int)size-1-(int)len;--i)
    result[i] = result[i-size+len];
  for (;i>=0;--i)
    result[i] = 0;
  return true;
}

static rfbBool HandleARDAuth(rfbClient *client)
{
  uint8_t gen[2], len[2];
  size_t keylen;
  uint8_t *mod = NULL, *resp, *pub, *key, *shared;
  gcry_mpi_t genmpi = NULL, modmpi = NULL, respmpi = NULL;
  gcry_mpi_t privmpi = NULL, pubmpi = NULL, keympi = NULL;
  gcry_md_hd_t md5 = NULL;
  gcry_cipher_hd_t aes = NULL;
  gcry_error_t error;
  uint8_t userpass[128], ciphertext[128];
  int passwordLen, usernameLen;
  rfbCredential *cred = NULL;
  rfbBool result = false;

  if (!gcry_control(GCRYCTL_INITIALIZATION_FINISHED_P))
  {
    /* Application did not initialize gcrypt, so we should */
    if (!gcry_check_version(GCRYPT_VERSION))
    {
      /* Older version of libgcrypt is installed on system than compiled against */
      rfbClientLog("libgcrypt version mismatch.\n");
    }
  }

  while (1)
  {
    if (!ReadFromRFBServer(client, (char *)gen, 2))
      break;
    if (!ReadFromRFBServer(client, (char *)len, 2))
      break;

    if (!client->GetCredential)
    {
      rfbClientLog("GetCredential callback is not set.\n");
      break;
    }
    cred = client->GetCredential(client, rfbCredentialTypeUser);
    if (!cred)
    {
      rfbClientLog("Reading credential failed\n");
      break;
    }

    keylen = 256*len[0]+len[1];
    mod = (uint8_t*)malloc(keylen*4);
    if (!mod)
    {
      rfbClientLog("malloc out of memory\n");
      break;
    }
    resp = mod+keylen;
    pub = resp+keylen;
    key = pub+keylen;

    if (!ReadFromRFBServer(client, (char *)mod, keylen))
      break;
    if (!ReadFromRFBServer(client, (char *)resp, keylen))
      break;

    error = gcry_mpi_scan(&genmpi, GCRYMPI_FMT_USG, gen, 2, NULL);
    if (gcry_err_code(error) != GPG_ERR_NO_ERROR)
    {
      rfbClientLog("gcry_mpi_scan error: %s\n", gcry_strerror(error));
      break;
    }
    error = gcry_mpi_scan(&modmpi, GCRYMPI_FMT_USG, mod, keylen, NULL);
    if (gcry_err_code(error) != GPG_ERR_NO_ERROR)
    {
      rfbClientLog("gcry_mpi_scan error: %s\n", gcry_strerror(error));
      break;
    }
    error = gcry_mpi_scan(&respmpi, GCRYMPI_FMT_USG, resp, keylen, NULL);
    if (gcry_err_code(error) != GPG_ERR_NO_ERROR)
    {
      rfbClientLog("gcry_mpi_scan error: %s\n", gcry_strerror(error));
      break;
    }

    privmpi = gcry_mpi_new(keylen);
    if (!privmpi)
    {
      rfbClientLog("gcry_mpi_new out of memory\n");
      break;
    }
    gcry_mpi_randomize(privmpi, (keylen/8)*8, GCRY_STRONG_RANDOM);

    pubmpi = gcry_mpi_new(keylen);
    if (!pubmpi)
    {
      rfbClientLog("gcry_mpi_new out of memory\n");
      break;
    }
    gcry_mpi_powm(pubmpi, genmpi, privmpi, modmpi);

    keympi = gcry_mpi_new(keylen);
    if (!keympi)
    {
      rfbClientLog("gcry_mpi_new out of memory\n");
      break;
    }
    gcry_mpi_powm(keympi, respmpi, privmpi, modmpi);

    if (!rfbMpiToBytes(pubmpi, pub, keylen))
      break;
    if (!rfbMpiToBytes(keympi, key, keylen))
      break;

    error = gcry_md_open(&md5, GCRY_MD_MD5, 0);
    if (gcry_err_code(error) != GPG_ERR_NO_ERROR)
    {
      rfbClientLog("gcry_md_open error: %s\n", gcry_strerror(error));
      break;
    }
    gcry_md_write(md5, key, keylen);
    error = gcry_md_final(md5);
    if (gcry_err_code(error) != GPG_ERR_NO_ERROR)
    {
      rfbClientLog("gcry_md_final error: %s\n", gcry_strerror(error));
      break;
    }
    shared = gcry_md_read(md5, GCRY_MD_MD5);

    passwordLen = strlen(cred->userCredential.password)+1;
    usernameLen = strlen(cred->userCredential.username)+1;
    if ((unsigned int)passwordLen > sizeof(userpass)/2)
      passwordLen = sizeof(userpass)/2;
    if ((unsigned int)usernameLen > sizeof(userpass)/2)
      usernameLen = sizeof(userpass)/2;

    gcry_randomize(userpass, sizeof(userpass), GCRY_STRONG_RANDOM);
    memcpy(userpass, cred->userCredential.username, usernameLen);
    memcpy(userpass+sizeof(userpass)/2, cred->userCredential.password, passwordLen);

    error = gcry_cipher_open(&aes, GCRY_CIPHER_AES128, GCRY_CIPHER_MODE_ECB, 0);
    if (gcry_err_code(error) != GPG_ERR_NO_ERROR)
    {
      rfbClientLog("gcry_cipher_open error: %s\n", gcry_strerror(error));
      break;
    }
    error = gcry_cipher_setkey(aes, shared, 16);
    if (gcry_err_code(error) != GPG_ERR_NO_ERROR)
    {
      rfbClientLog("gcry_cipher_setkey error: %s\n", gcry_strerror(error));
      break;
    }
    error = gcry_cipher_encrypt(aes, ciphertext, sizeof(ciphertext), userpass, sizeof(userpass));
    if (gcry_err_code(error) != GPG_ERR_NO_ERROR)
    {
      rfbClientLog("gcry_cipher_encrypt error: %s\n", gcry_strerror(error));
      break;
    }

    if (!WriteToRFBServer(client, (char *)ciphertext, sizeof(ciphertext)))
      break;
    if (!WriteToRFBServer(client, (char *)pub, keylen))
      break;

    /* Handle the SecurityResult message */
    if (!rfbHandleAuthResult(client))
      break;

    result = true;
    break;
  }

  if (cred)
    FreeUserCredential(cred);
  if (mod)
    free(mod);
  if (genmpi)
    gcry_mpi_release(genmpi);
  if (modmpi)
    gcry_mpi_release(modmpi);
  if (respmpi)
    gcry_mpi_release(respmpi);
  if (privmpi)
    gcry_mpi_release(privmpi);
  if (pubmpi)
    gcry_mpi_release(pubmpi);
  if (keympi)
    gcry_mpi_release(keympi);
  if (md5)
    gcry_md_close(md5);
  if (aes)
    gcry_cipher_close(aes);
  return result;
}
#endif

/*
 * SetClientAuthSchemes.
 */

void SetClientAuthSchemes(rfbClient* client,const uint32_t *authSchemes, int size)
{
  int i;

  if (client->clientAuthSchemes)
  {
    free(client->clientAuthSchemes);
    client->clientAuthSchemes = NULL;
  }
  if (authSchemes)
  {
    if (size<0)
    {
      /* If size<0 we assume the passed-in list is also 0-terminate, so we
       * calculate the size here */
      for (size=0;authSchemes[size];size++) ;
    }
    client->clientAuthSchemes = (uint32_t*)malloc(sizeof(uint32_t)*(size+1));
    for (i=0;i<size;i++)
      client->clientAuthSchemes[i] = authSchemes[i];
    client->clientAuthSchemes[size] = 0;
  }
}

/*
 * InitialiseRFBConnection.
 */

rfbBool InitialiseRFBConnection(rfbClient* client)
{
  rfbProtocolVersionMsg pv;
  int major,minor;
  uint32_t authScheme;
  uint32_t subAuthScheme;
  rfbClientInitMsg ci;

  /* if the connection is immediately closed, don't report anything, so
       that pmw's monitor can make test connections */

  if (client->listenSpecified)
    errorMessageOnReadFailure = false;

  if (!ReadFromRFBServer(client, pv, sz_rfbProtocolVersionMsg)) return false;
  pv[sz_rfbProtocolVersionMsg]=0;

  errorMessageOnReadFailure = true;

  pv[sz_rfbProtocolVersionMsg] = 0;

  if (sscanf(pv,rfbProtocolVersionFormat,&major,&minor) != 2) {
    rfbClientLog("Not a valid VNC server (%s)\n",pv);
    return false;
  }


  DefaultSupportedMessages(client);
  client->major = major;
  client->minor = minor;

  /* fall back to viewer supported version */
  if ((major==rfbProtocolMajorVersion) && (minor>rfbProtocolMinorVersion))
    client->minor = rfbProtocolMinorVersion;

  /* UltraVNC uses minor codes 4 and 6 for the server */
  if (major==3 && (minor==4 || minor==6)) {
      rfbClientLog("UltraVNC server detected, enabling UltraVNC specific messages\n",pv);
      DefaultSupportedMessagesUltraVNC(client);
  }

  /* UltraVNC Single Click uses minor codes 14 and 16 for the server */
  if (major==3 && (minor==14 || minor==16)) {
     minor = minor - 10;
     client->minor = minor;
     rfbClientLog("UltraVNC Single Click server detected, enabling UltraVNC specific messages\n",pv);
     DefaultSupportedMessagesUltraVNC(client);
  }

  /* TightVNC uses minor codes 5 for the server */
  if (major==3 && minor==5) {
      rfbClientLog("TightVNC server detected, enabling TightVNC specific messages\n",pv);
      DefaultSupportedMessagesTightVNC(client);
  }

  /* we do not support > RFB3.8 */
  if ((major==3 && minor>8) || major>3)
  {
    client->major=3;
    client->minor=8;
  }

  rfbClientLog("VNC server supports protocol version %d.%d (viewer %d.%d)\n",
	  major, minor, rfbProtocolMajorVersion, rfbProtocolMinorVersion);

  sprintf(pv,rfbProtocolVersionFormat,client->major,client->minor);

  if (!WriteToRFBServer(client, pv, sz_rfbProtocolVersionMsg)) return false;


  /* 3.7 and onwards sends a # of security types first */
  if (client->major==3 && client->minor > 6)
  {
    if (!ReadSupportedSecurityType(client, &authScheme, false)) return false;
  }
  else
  {
    if (!ReadFromRFBServer(client, (char *)&authScheme, 4)) return false;
    authScheme = rfbClientSwap32IfLE(authScheme);
  }
  
  rfbClientLog("Selected Security Scheme %d\n", authScheme);
  client->authScheme = authScheme;
  
  switch (authScheme) {

  case rfbConnFailed:
    ReadReason(client);
    return false;

  case rfbNoAuth:
    rfbClientLog("No authentication needed\n");

    /* 3.8 and upwards sends a Security Result for rfbNoAuth */
    if ((client->major==3 && client->minor > 7) || client->major>3)
        if (!rfbHandleAuthResult(client)) return false;

    break;

  case rfbVncAuth:
    if (!HandleVncAuth(client)) return false;
    break;

  case rfbMSLogon:
    if (!HandleMSLogonAuth(client)) return false;
    break;

  case rfbARD:
#ifndef LIBVNCSERVER_WITH_CLIENT_GCRYPT
    rfbClientLog("GCrypt support was not compiled in\n");
    return false;
#else
    if (!HandleARDAuth(client)) return false;
#endif
    break;

  case rfbTLS:
    if (!HandleAnonTLSAuth(client)) return false;
    /* After the TLS session is established, sub auth types are expected.
     * Note that all following reading/writing are through the TLS session from here.
     */
    if (!ReadSupportedSecurityType(client, &subAuthScheme, true)) return false;
    client->subAuthScheme = subAuthScheme;

    switch (subAuthScheme) {

      case rfbConnFailed:
        ReadReason(client);
        return false;

      case rfbNoAuth:
        rfbClientLog("No sub authentication needed\n");
        /* 3.8 and upwards sends a Security Result for rfbNoAuth */
        if ((client->major==3 && client->minor > 7) || client->major>3)
            if (!rfbHandleAuthResult(client)) return false;
        break;

      case rfbVncAuth:
        if (!HandleVncAuth(client)) return false;
        break;

      default:
        rfbClientLog("Unknown sub authentication scheme from VNC server: %d\n",
            (int)subAuthScheme);
        return false;
    }

    break;

  case rfbVeNCrypt:
    if (!HandleVeNCryptAuth(client)) return false;

    switch (client->subAuthScheme) {

      case rfbVeNCryptTLSNone:
      case rfbVeNCryptX509None:
        rfbClientLog("No sub authentication needed\n");
        if (!rfbHandleAuthResult(client)) return false;
        break;

      case rfbVeNCryptTLSVNC:
      case rfbVeNCryptX509VNC:
        if (!HandleVncAuth(client)) return false;
        break;

      case rfbVeNCryptTLSPlain:
      case rfbVeNCryptX509Plain:
        if (!HandlePlainAuth(client)) return false;
        break;

      default:
        rfbClientLog("Unknown sub authentication scheme from VNC server: %d\n",
            client->subAuthScheme);
        return false;
    }

    break;

  default:
    rfbClientLog("Unknown authentication scheme from VNC server: %d\n",
	    (int)authScheme);
    return false;
  }

  ci.shared = (client->appData.shareDesktop ? 1 : 0);

  if (!WriteToRFBServer(client,  (char *)&ci, sz_rfbClientInitMsg)) return false;

  if (!ReadFromRFBServer(client, (char *)&client->si, sz_rfbServerInitMsg)) return false;

  client->si.framebufferWidth = rfbClientSwap16IfLE(client->si.framebufferWidth);
  client->si.framebufferHeight = rfbClientSwap16IfLE(client->si.framebufferHeight);
  client->si.format.redMax = rfbClientSwap16IfLE(client->si.format.redMax);
  client->si.format.greenMax = rfbClientSwap16IfLE(client->si.format.greenMax);
  client->si.format.blueMax = rfbClientSwap16IfLE(client->si.format.blueMax);
  client->si.nameLength = rfbClientSwap32IfLE(client->si.nameLength);

  /* To guard against integer wrap-around, si.nameLength is cast to 64 bit */
  client->desktopName = malloc((uint64_t)client->si.nameLength + 1);
  if (!client->desktopName) {
    rfbClientLog("Error allocating memory for desktop name, %lu bytes\n",
            (unsigned long)client->si.nameLength);
    return false;
  }

  if (!ReadFromRFBServer(client, client->desktopName, client->si.nameLength)) return false;

  client->desktopName[client->si.nameLength] = 0;

  rfbClientLog("Desktop name \"%s\"\n",client->desktopName);

  rfbClientLog("Connected to VNC server, using protocol version %d.%d\n",
	  client->major, client->minor);

  rfbClientLog("VNC server default format:\n");
  PrintPixelFormat(&client->si.format);

  return true;
}


/*
 * SetFormatAndEncodings.
 */

rfbBool SetFormatAndEncodings(rfbClient* client)
{
  rfbSetPixelFormatMsg spf;
  char buf[sz_rfbSetEncodingsMsg + MAX_ENCODINGS * 4];

  rfbSetEncodingsMsg *se = (rfbSetEncodingsMsg *)buf;
  uint32_t *encs = (uint32_t *)(&buf[sz_rfbSetEncodingsMsg]);
  int len = 0;
  rfbBool requestCompressLevel = false;
  rfbBool requestQualityLevel = false;
  rfbBool requestLastRectEncoding = false;
  rfbClientProtocolExtension* e;

  if (!SupportsClient2Server(client, rfbSetPixelFormat)) return true;

  spf.type = rfbSetPixelFormat;
  spf.pad1 = 0;
  spf.pad2 = 0;
  spf.format = client->format;
  spf.format.redMax = rfbClientSwap16IfLE(spf.format.redMax);
  spf.format.greenMax = rfbClientSwap16IfLE(spf.format.greenMax);
  spf.format.blueMax = rfbClientSwap16IfLE(spf.format.blueMax);

  if (!WriteToRFBServer(client, (char *)&spf, sz_rfbSetPixelFormatMsg))
      return false;

  if (!SupportsClient2Server(client, rfbSetEncodings))
      return true;

  se->type = rfbSetEncodings;
  se->nEncodings = 0;

  if (client->appData.encodingsString) {
    const char *encStr = client->appData.encodingsString;
    int encStrLen;
    do {
      const char *nextEncStr = strchr(encStr, ' ');
      if (nextEncStr) {
	encStrLen = nextEncStr - encStr;
	nextEncStr++;
      } else {
	encStrLen = strlen(encStr);
      }

      if (strncasecmp(encStr,"raw",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingRaw);
      } else if (strncasecmp(encStr,"copyrect",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingCopyRect);
#ifdef LIBVNCSERVER_HAVE_LIBZ
#ifdef LIBVNCSERVER_HAVE_LIBJPEG
      } else if (strncasecmp(encStr,"tight",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingTight);
    requestLastRectEncoding = true;
	if (client->appData.compressLevel >= 0 && client->appData.compressLevel <= 9)
      requestCompressLevel = true;
	if (client->appData.enableJPEG)
      requestQualityLevel = true;
#endif
#endif
      } else if (strncasecmp(encStr,"hextile",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingHextile);
#ifdef LIBVNCSERVER_HAVE_LIBZ
      } else if (strncasecmp(encStr,"zlib",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingZlib);
	if (client->appData.compressLevel >= 0 && client->appData.compressLevel <= 9)
      requestCompressLevel = true;
      } else if (strncasecmp(encStr,"zlibhex",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingZlibHex);
	if (client->appData.compressLevel >= 0 && client->appData.compressLevel <= 9)
      requestCompressLevel = true;
      } else if (strncasecmp(encStr,"zrle",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingZRLE);
      } else if (strncasecmp(encStr,"zywrle",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingZYWRLE);
    requestQualityLevel = true;
#endif
      } else if ((strncasecmp(encStr,"ultra",encStrLen) == 0) || (strncasecmp(encStr,"ultrazip",encStrLen) == 0)) {
        /* There are 2 encodings used in 'ultra' */
        encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingUltra);
        encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingUltraZip);
      } else if (strncasecmp(encStr,"corre",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingCoRRE);
      } else if (strncasecmp(encStr,"rre",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingRRE);
#ifdef LIBVNCSERVER_CONFIG_LIBVA
      } else if (strncasecmp(encStr,"h264",encStrLen) == 0) {
	encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingH264);
#endif
      } else {
	rfbClientLog("Unknown encoding '%.*s'\n",encStrLen,encStr);
      }

      encStr = nextEncStr;
    } while (encStr && se->nEncodings < MAX_ENCODINGS);

    if (se->nEncodings < MAX_ENCODINGS && requestCompressLevel) {
      encs[se->nEncodings++] = rfbClientSwap32IfLE(client->appData.compressLevel +
					  rfbEncodingCompressLevel0);
    }

    if (se->nEncodings < MAX_ENCODINGS && requestQualityLevel) {
      if (client->appData.qualityLevel < 0 || client->appData.qualityLevel > 9)
        client->appData.qualityLevel = 5;
      encs[se->nEncodings++] = rfbClientSwap32IfLE(client->appData.qualityLevel +
					  rfbEncodingQualityLevel0);
    }
  }
  else {
    if (SameMachine(client->sock)) {
      /* TODO:
      if (!tunnelSpecified) {
      */
      rfbClientLog("Same machine: preferring raw encoding\n");
      encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingRaw);
      /*
      } else {
	rfbClientLog("Tunneling active: preferring tight encoding\n");
      }
      */
    }

    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingCopyRect);
#ifdef LIBVNCSERVER_HAVE_LIBZ
#ifdef LIBVNCSERVER_HAVE_LIBJPEG
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingTight);
    requestLastRectEncoding = true;
#endif
#endif
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingHextile);
#ifdef LIBVNCSERVER_HAVE_LIBZ
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingZlib);
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingZRLE);
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingZYWRLE);
#endif
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingUltra);
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingUltraZip);
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingCoRRE);
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingRRE);

    if (client->appData.compressLevel >= 0 && client->appData.compressLevel <= 9) {
      encs[se->nEncodings++] = rfbClientSwap32IfLE(client->appData.compressLevel +
					  rfbEncodingCompressLevel0);
    } else /* if (!tunnelSpecified) */ {
      /* If -tunnel option was provided, we assume that server machine is
	 not in the local network so we use default compression level for
	 tight encoding instead of fast compression. Thus we are
	 requesting level 1 compression only if tunneling is not used. */
      encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingCompressLevel1);
    }

    if (client->appData.enableJPEG) {
      if (client->appData.qualityLevel < 0 || client->appData.qualityLevel > 9)
	client->appData.qualityLevel = 5;
      encs[se->nEncodings++] = rfbClientSwap32IfLE(client->appData.qualityLevel +
					  rfbEncodingQualityLevel0);
    }
#ifdef LIBVNCSERVER_CONFIG_LIBVA
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingH264);
    rfbClientLog("h264 encoding added\n");
#endif
  }

  /* Remote Cursor Support (local to viewer) */
  if (client->appData.useRemoteCursor) {
    if (se->nEncodings < MAX_ENCODINGS)
      encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingXCursor);
    if (se->nEncodings < MAX_ENCODINGS)
      encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingRichCursor);
    if (se->nEncodings < MAX_ENCODINGS)
      encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingPointerPos);
  }

  /* Keyboard State Encodings */
  if (se->nEncodings < MAX_ENCODINGS)
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingKeyboardLedState);

  /* New Frame Buffer Size */
  if (se->nEncodings < MAX_ENCODINGS && client->canHandleNewFBSize)
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingNewFBSize);

  /* Last Rect */
  if (se->nEncodings < MAX_ENCODINGS && requestLastRectEncoding)
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingLastRect);

  /* Server Capabilities */
  if (se->nEncodings < MAX_ENCODINGS)
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingSupportedMessages);
  if (se->nEncodings < MAX_ENCODINGS)
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingSupportedEncodings);
  if (se->nEncodings < MAX_ENCODINGS)
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingServerIdentity);

  /* xvp */
  if (se->nEncodings < MAX_ENCODINGS)
    encs[se->nEncodings++] = rfbClientSwap32IfLE(rfbEncodingXvp);

  /* client extensions */
  for(e = rfbClientExtensions; e; e = e->next)
    if(e->encodings) {
      int* enc;
      for(enc = e->encodings; *enc; enc++)
	encs[se->nEncodings++] = rfbClientSwap32IfLE(*enc);
    }

  len = sz_rfbSetEncodingsMsg + se->nEncodings * 4;

  se->nEncodings = rfbClientSwap16IfLE(se->nEncodings);

  if (!WriteToRFBServer(client, buf, len)) return false;

  return true;
}

/*
 * SendIncrementalFramebufferUpdateRequest.
 */

rfbBool SendIncrementalFramebufferUpdateRequest(rfbClient* client)
{
	return SendFramebufferUpdateRequest(client,
			client->updateRect.x, client->updateRect.y,
            client->updateRect.w, client->updateRect.h, true);
}

/*
 * SendFramebufferUpdateRequest.
 */

rfbBool SendFramebufferUpdateRequest(rfbClient* client, int x, int y, int w, int h, rfbBool incremental)
{
  rfbFramebufferUpdateRequestMsg fur;

  if (!SupportsClient2Server(client, rfbFramebufferUpdateRequest)) return true;
  
  fur.type = rfbFramebufferUpdateRequest;
  fur.incremental = incremental ? 1 : 0;
  fur.x = rfbClientSwap16IfLE(x);
  fur.y = rfbClientSwap16IfLE(y);
  fur.w = rfbClientSwap16IfLE(w);
  fur.h = rfbClientSwap16IfLE(h);

  if (!WriteToRFBServer(client, (char *)&fur, sz_rfbFramebufferUpdateRequestMsg))
    return false;

  return true;
}

/*
 * SendScaleSetting.
 */
rfbBool SendScaleSetting(rfbClient* client,int scaleSetting)
{
  rfbSetScaleMsg ssm;

  ssm.scale = scaleSetting;
  ssm.pad = 0;
  
  /* favor UltraVNC SetScale if both are supported */
  if (SupportsClient2Server(client, rfbSetScale)) {
      ssm.type = rfbSetScale;
      if (!WriteToRFBServer(client, (char *)&ssm, sz_rfbSetScaleMsg))
          return false;
  }
  
  if (SupportsClient2Server(client, rfbPalmVNCSetScaleFactor)) {
      ssm.type = rfbPalmVNCSetScaleFactor;
      if (!WriteToRFBServer(client, (char *)&ssm, sz_rfbSetScaleMsg))
          return false;
  }

  return true;
}

/*
 * TextChatFunctions (UltraVNC)
 * Extremely bandwidth friendly method of communicating with a user
 * (Think HelpDesk type applications)
 */

rfbBool TextChatSend(rfbClient* client, char *text)
{
    rfbTextChatMsg chat;
    int count = strlen(text);

    if (!SupportsClient2Server(client, rfbTextChat)) return true;
    chat.type = rfbTextChat;
    chat.pad1 = 0;
    chat.pad2 = 0;
    chat.length = (uint32_t)count;
    chat.length = rfbClientSwap32IfLE(chat.length);

    if (!WriteToRFBServer(client, (char *)&chat, sz_rfbTextChatMsg))
        return false;

    if (count>0) {
        if (!WriteToRFBServer(client, text, count))
            return false;
    }
    return true;
}

rfbBool TextChatOpen(rfbClient* client)
{
    rfbTextChatMsg chat;

    if (!SupportsClient2Server(client, rfbTextChat)) return true;
    chat.type = rfbTextChat;
    chat.pad1 = 0;
    chat.pad2 = 0;
    chat.length = rfbClientSwap32IfLE(rfbTextChatOpen);
    return  (WriteToRFBServer(client, (char *)&chat, sz_rfbTextChatMsg) ? true : false);
}

rfbBool TextChatClose(rfbClient* client)
{
    rfbTextChatMsg chat;
    if (!SupportsClient2Server(client, rfbTextChat)) return true;
    chat.type = rfbTextChat;
    chat.pad1 = 0;
    chat.pad2 = 0;
    chat.length = rfbClientSwap32IfLE(rfbTextChatClose);
    return  (WriteToRFBServer(client, (char *)&chat, sz_rfbTextChatMsg) ? true : false);
}

rfbBool TextChatFinish(rfbClient* client)
{
    rfbTextChatMsg chat;
    if (!SupportsClient2Server(client, rfbTextChat)) return true;
    chat.type = rfbTextChat;
    chat.pad1 = 0;
    chat.pad2 = 0;
    chat.length = rfbClientSwap32IfLE(rfbTextChatFinished);
    return  (WriteToRFBServer(client, (char *)&chat, sz_rfbTextChatMsg) ? true : false);
}

/*
 * UltraVNC Server Input Disable
 * Apparently, the remote client can *prevent* the local user from interacting with the display
 * I would think this is extremely helpful when used in a HelpDesk situation
 */
rfbBool PermitServerInput(rfbClient* client, int enabled)
{
    rfbSetServerInputMsg msg;

    if (!SupportsClient2Server(client, rfbSetServerInput)) return true;
    /* enabled==1, then server input from local keyboard is disabled */
    msg.type = rfbSetServerInput;
    msg.status = (enabled ? 1 : 0);
    msg.pad = 0;
    return  (WriteToRFBServer(client, (char *)&msg, sz_rfbSetServerInputMsg) ? true : false);
}

/*
 * send xvp client message
 * A client supporting the xvp extension sends this to request that the server initiate
 * a clean shutdown, clean reboot or abrupt reset of the system whose framebuffer the
 * client is displaying.
 *
 * only version 1 is defined in the protocol specs
 *
 * possible values for code are:
 *   rfbXvp_Shutdown
 *   rfbXvp_Reboot
 *   rfbXvp_Reset
 */

rfbBool SendXvpMsg(rfbClient* client, uint8_t version, uint8_t code)
{
    rfbXvpMsg xvp;

    if (!SupportsClient2Server(client, rfbXvp)) return true;
    xvp.type = rfbXvp;
    xvp.pad = 0;
    xvp.version = version;
    xvp.code = code;

    if (!WriteToRFBServer(client, (char *)&xvp, sz_rfbXvpMsg))
        return false;

    return true;
}

/*
 * SendPointerEvent.
 */

rfbBool SendPointerEvent(rfbClient* client,int x, int y, int buttonMask)
{
  rfbPointerEventMsg pe;

  if (!SupportsClient2Server(client, rfbPointerEvent)) return true;

  pe.type = rfbPointerEvent;
  pe.buttonMask = buttonMask;
  if (x < 0) x = 0;
  if (y < 0) y = 0;

  pe.x = rfbClientSwap16IfLE(x);
  pe.y = rfbClientSwap16IfLE(y);
  return WriteToRFBServer(client, (char *)&pe, sz_rfbPointerEventMsg);
}

/*
 * SendKeyEvent.
 */

rfbBool SendKeyEvent(rfbClient* client, uint32_t key, rfbBool down)
{
  rfbKeyEventMsg ke;

  if (!SupportsClient2Server(client, rfbKeyEvent)) return true;

  ke.type = rfbKeyEvent;
  ke.down = down ? 1 : 0;
  ke.key = rfbClientSwap32IfLE(key);
  return WriteToRFBServer(client, (char *)&ke, sz_rfbKeyEventMsg);
}

/*
 * SendClientCutText.
 */

rfbBool SendClientCutText(rfbClient* client, char *str, int len)
{
  rfbClientCutTextMsg cct;

  if (!SupportsClient2Server(client, rfbClientCutText)) return true;

  cct.type = rfbClientCutText;
  cct.length = rfbClientSwap32IfLE(len);
  return  (WriteToRFBServer(client, (char *)&cct, sz_rfbClientCutTextMsg) &&
	   WriteToRFBServer(client, str, len));
}

/*
 * HandleRFBServerMessage.
 */

rfbBool HandleRFBServerMessage(rfbClient *client)
{
  rfbServerToClientMsg msg;

  if (client->serverPort==-1)
    client->vncRec->readTimestamp = true;
  if (!ReadFromRFBServer(client, (char *)&msg, 1))
    return false;

  switch (msg.type) {
  case rfbSetColourMapEntries: {
      /* TODO:
      int i;
      uint16_t rgb[3];
      XColor xc;

      if (!ReadFromRFBServer(client, ((char *)&msg) + 1,
                 sz_rfbSetColourMapEntriesMsg - 1))
        return false;

      msg.scme.firstColour = rfbClientSwap16IfLE(msg.scme.firstColour);
      msg.scme.nColours = rfbClientSwap16IfLE(msg.scme.nColours);

      for (i = 0; i < msg.scme.nColours; i++) {
        if (!ReadFromRFBServer(client, (char *)rgb, 6))
      return false;
        xc.pixel = msg.scme.firstColour + i;
        xc.red = rfbClientSwap16IfLE(rgb[0]);
        xc.green = rfbClientSwap16IfLE(rgb[1]);
        xc.blue = rfbClientSwap16IfLE(rgb[2]);
        xc.flags = DoRed|DoGreen|DoBlue;
        XStoreColor(dpy, cmap, &xc);
      }
      */
      }
      break;
  case rfbFramebufferUpdate: {
    rfbFramebufferUpdateRectHeader rect;
    int linesToRead;
    int bytesPerLine;
    int i;

    if (!ReadFromRFBServer(client, ((char *)&msg.fu) + 1, sz_rfbFramebufferUpdateMsg - 1))
        return false;
    msg.fu.nRects = rfbClientSwap16IfLE(msg.fu.nRects);
    for (i = 0; i < msg.fu.nRects; i++) {
        if (!ReadFromRFBServer(client, (char *)&rect, sz_rfbFramebufferUpdateRectHeader))
            return false;

        rect.encoding = rfbClientSwap32IfLE(rect.encoding);
        if (rect.encoding == rfbEncodingLastRect)
            break;

        rect.r.x = rfbClientSwap16IfLE(rect.r.x);
        rect.r.y = rfbClientSwap16IfLE(rect.r.y);
        rect.r.w = rfbClientSwap16IfLE(rect.r.w);
        rect.r.h = rfbClientSwap16IfLE(rect.r.h);

        if (rect.encoding == rfbEncodingXCursor || rect.encoding == rfbEncodingRichCursor) {
            if (!HandleCursorShape(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h, rect.encoding))
                return false;
            continue;
        }

        if (rect.encoding == rfbEncodingPointerPos) {
            if (!client->HandleCursorPos(client,rect.r.x, rect.r.y)) {
                return false;
            }
            continue;
        }

        if (rect.encoding == rfbEncodingKeyboardLedState) {
            /* OK! We have received a keyboard state message!!! */
            client->KeyboardLedStateEnabled = 1;
            if (client->HandleKeyboardLedState!=NULL)
                client->HandleKeyboardLedState(client, rect.r.x, 0);
            /* stash it for the future */
            client->CurrentKeyboardLedState = rect.r.x;
            continue;
        }

        if (rect.encoding == rfbEncodingNewFBSize) {
            client->width = rect.r.w;
            client->height = rect.r.h;
            client->updateRect.x = client->updateRect.y = 0;
            client->updateRect.w = client->width;
            client->updateRect.h = client->height;
            if (!client->MallocFrameBuffer(client))
                return false;
            SendFramebufferUpdateRequest(client, 0, 0, rect.r.w, rect.r.h, false);
            rfbClientLog("Got new framebuffer size: %dx%d\n", rect.r.w, rect.r.h);
            continue;
        }

        /* rect.r.w=byte count */
        if (rect.encoding == rfbEncodingSupportedMessages) {
            int loop;
            if (!ReadFromRFBServer(client, (char *)&client->supportedMessages, sz_rfbSupportedMessages))
                return false;
            /* msgs is two sets of bit flags of supported messages client2server[] and server2client[] */
            /* currently ignored by this library */
            rfbClientLog("client2server supported messages (bit flags)\n");
            for (loop=0;loop<32;loop+=8)
                rfbClientLog("%02X: %04x %04x %04x %04x - %04x %04x %04x %04x\n", loop,
                    client->supportedMessages.client2server[loop],   client->supportedMessages.client2server[loop+1],
                    client->supportedMessages.client2server[loop+2], client->supportedMessages.client2server[loop+3],
                    client->supportedMessages.client2server[loop+4], client->supportedMessages.client2server[loop+5],
                    client->supportedMessages.client2server[loop+6], client->supportedMessages.client2server[loop+7]);

            rfbClientLog("server2client supported messages (bit flags)\n");
            for (loop=0;loop<32;loop+=8)
                rfbClientLog("%02X: %04x %04x %04x %04x - %04x %04x %04x %04x\n", loop,
                    client->supportedMessages.server2client[loop],   client->supportedMessages.server2client[loop+1],
                    client->supportedMessages.server2client[loop+2], client->supportedMessages.server2client[loop+3],
                    client->supportedMessages.server2client[loop+4], client->supportedMessages.server2client[loop+5],
                    client->supportedMessages.server2client[loop+6], client->supportedMessages.server2client[loop+7]);
            continue;
        }

        /* rect.r.w=byte count, rect.r.h=# of encodings */
        if (rect.encoding == rfbEncodingSupportedEncodings) {
            char *buffer;
            buffer = malloc(rect.r.w);
            if (!ReadFromRFBServer(client, buffer, rect.r.w)) {
                free(buffer);
                return false;
            }

            /* buffer now contains rect.r.h # of uint32_t encodings that the server supports */
            /* currently ignored by this library */
            free(buffer);
            continue;
        }

        /* rect.r.w=byte count */
        if (rect.encoding == rfbEncodingServerIdentity) {
            char *buffer;
            buffer = malloc(rect.r.w+1);
            if (!ReadFromRFBServer(client, buffer, rect.r.w)) {
                free(buffer);
                return false;
            }
            buffer[rect.r.w]=0; /* null terminate, just in case */
            rfbClientLog("Server identity: \"%s\"\n", buffer);
            free(buffer);
            continue;
        }

        /* rfbEncodingUltraZip is a collection of subrects.   x = # of subrects, and h is always 0 */
        if (rect.encoding != rfbEncodingUltraZip) {
            if ((rect.r.x + rect.r.w > client->width) || (rect.r.y + rect.r.h > client->height)) {
                rfbClientLog("Rect too large: %dx%d at (%d, %d)\n",
                rect.r.w, rect.r.h, rect.r.x, rect.r.y);
                return false;
            }
            client->SoftCursorLockArea(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h);
        }

        switch (rect.encoding) {
        case rfbEncodingRaw: {
                int y=rect.r.y, h=rect.r.h;
                bytesPerLine = rect.r.w * client->format.bitsPerPixel / 8;
                linesToRead = RFB_BUFFER_SIZE / bytesPerLine;
                while (h > 0) {
                    if (linesToRead > h)
                        linesToRead = h;
                    if (!ReadFromRFBServer(client, client->buffer,bytesPerLine * linesToRead))
                        return false;
                    CopyRectangle(client, (uint8_t *)client->buffer, rect.r.x, y, rect.r.w,linesToRead);
                    h -= linesToRead;
                    y += linesToRead;
                }
            }
            break;
        case rfbEncodingCopyRect: {
                rfbCopyRect cr;
                if (!ReadFromRFBServer(client, (char *)&cr, sz_rfbCopyRect))
                    return false;
                cr.srcX = rfbClientSwap16IfLE(cr.srcX);
                cr.srcY = rfbClientSwap16IfLE(cr.srcY);
                /* If RichCursor encoding is used, we should extend our
                   "cursor lock area" (previously set to destination
                   rectangle) to the source rectangle as well. */
                client->SoftCursorLockArea(client, cr.srcX, cr.srcY, rect.r.w, rect.r.h);
                if (client->GotCopyRect != NULL)
                    client->GotCopyRect(client, cr.srcX, cr.srcY, rect.r.w, rect.r.h, rect.r.x, rect.r.y);
                else
                    CopyRectangleFromRectangle(client, cr.srcX, cr.srcY, rect.r.w, rect.r.h, rect.r.x, rect.r.y);
            }
            break;
        case rfbEncodingRRE: {
                switch (client->format.bitsPerPixel) {
                case 8:
                    if (!HandleRRE8(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 16:
                    if (!HandleRRE16(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 32:
                    if (!HandleRRE32(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                }
            }
            break;
        case rfbEncodingCoRRE: {
                switch (client->format.bitsPerPixel) {
                case 8:
                    if (!HandleCoRRE8(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 16:
                    if (!HandleCoRRE16(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 32:
                    if (!HandleCoRRE32(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                }
            }
            break;
        case rfbEncodingHextile: {
                switch (client->format.bitsPerPixel) {
                case 8:
                    if (!HandleHextile8(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 16:
                    if (!HandleHextile16(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 32:
                    if (!HandleHextile32(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                }
            }
            break;
        case rfbEncodingUltra: {
                switch (client->format.bitsPerPixel) {
                case 8:
                    if (!HandleUltra8(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 16:
                    if (!HandleUltra16(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 32:
                    if (!HandleUltra32(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                }
            }
            break;
        case rfbEncodingUltraZip: {
                switch (client->format.bitsPerPixel) {
                case 8:
                    if (!HandleUltraZip8(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 16:
                    if (!HandleUltraZip16(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 32:
                    if (!HandleUltraZip32(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                }
            }
            break;
#ifdef LIBVNCSERVER_HAVE_LIBZ
        case rfbEncodingZlib: {
                switch (client->format.bitsPerPixel) {
                case 8:
                    if (!HandleZlib8(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 16:
                    if (!HandleZlib16(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 32:
                    if (!HandleZlib32(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                }
            }
            break;
#ifdef LIBVNCSERVER_HAVE_LIBJPEG
        case rfbEncodingTight: {
                switch (client->format.bitsPerPixel) {
                case 8:
                    if (!HandleTight8(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 16:
                    if (!HandleTight16(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                case 32:
                    if (!HandleTight32(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                        return false;
                    break;
                }
            }
            break;
#endif
        case rfbEncodingZRLE:
                client->appData.qualityLevel = 9;
        case rfbEncodingZYWRLE: {
                switch (client->format.bitsPerPixel) {
                case 8:
                    if (!HandleZRLE8(client, rect.r.x,rect.r.y,rect.r.w,rect.r.h))
                        return false;
                    break;
                case 16:
                    if (client->si.format.greenMax > 0x1F) {
                        if (!HandleZRLE16(client, rect.r.x,rect.r.y,rect.r.w,rect.r.h))
                            return false;
                    } else {
                        if (!HandleZRLE15(client, rect.r.x,rect.r.y,rect.r.w,rect.r.h))
                            return false;
                    }
                    break;
                case 32: {
                        uint32_t maxColor = (client->format.redMax<<client->format.redShift) | (client->format.greenMax<<client->format.greenShift) | (client->format.blueMax<<client->format.blueShift);
                        if ((client->format.bigEndian && (maxColor&0xff)==0) || (!client->format.bigEndian && (maxColor&0xff000000)==0)) {
                            if (!HandleZRLE24(client, rect.r.x,rect.r.y,rect.r.w,rect.r.h))
                                return false;
                        } else if (!client->format.bigEndian && (maxColor&0xff)==0) {
                            if (!HandleZRLE24Up(client, rect.r.x,rect.r.y,rect.r.w,rect.r.h))
                                return false;
                        } else if (client->format.bigEndian && (maxColor&0xff000000)==0) {
                            if (!HandleZRLE24Down(client, rect.r.x,rect.r.y,rect.r.w,rect.r.h))
                                return false;
                        } else if (!HandleZRLE32(client, rect.r.x,rect.r.y,rect.r.w,rect.r.h))
                            return false;
                    }
                    break;
                }
            }
            break;
#endif
#ifdef LIBVNCSERVER_CONFIG_LIBVA
        case rfbEncodingH264: {
                if (!HandleH264(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h))
                    return false;
            }
            break;
#endif
        default: {
                rfbBool handled = false;
                rfbClientProtocolExtension* e;
                for(e = rfbClientExtensions; !handled && e; e = e->next)
                    if(e->handleEncoding && e->handleEncoding(client, &rect))
                        handled = true;
                if(!handled) {
                    rfbClientLog("Unknown rect encoding %d\n", (int)rect.encoding);
                    return false;
                }
            }
            break;
        }
        /* Now we may discard "soft cursor locks". */
        client->SoftCursorUnlockScreen(client);
        client->GotFrameBufferUpdate(client, rect.r.x, rect.r.y, rect.r.w, rect.r.h);
    }
    if (!SendIncrementalFramebufferUpdateRequest(client))
        return false;
    if (client->FinishedFrameBufferUpdate)
        client->FinishedFrameBufferUpdate(client);
    break;
  }
  case rfbBell: {
          client->Bell(client);
      }
      break;
  case rfbServerCutText: {
          char *buffer;
          if (!ReadFromRFBServer(client, ((char *)&msg) + 1, sz_rfbServerCutTextMsg - 1))
              return false;
          msg.sct.length = rfbClientSwap32IfLE(msg.sct.length);
          buffer = malloc(msg.sct.length+1);
          if (!ReadFromRFBServer(client, buffer, msg.sct.length))
              return false;
          buffer[msg.sct.length] = 0;
          if (client->GotXCutText)
              client->GotXCutText(client, buffer, msg.sct.length);
          free(buffer);
      }
      break;
  case rfbTextChat: {
          char *buffer=NULL;
          if (!ReadFromRFBServer(client, ((char *)&msg) + 1, sz_rfbTextChatMsg- 1))
              return false;
          msg.tc.length = rfbClientSwap32IfLE(msg.sct.length);
          switch(msg.tc.length) {
          case rfbTextChatOpen:
              rfbClientLog("Received TextChat Open\n");
              if (client->HandleTextChat!=NULL)
                  client->HandleTextChat(client, (int)rfbTextChatOpen, NULL);
              break;
          case rfbTextChatClose:
              rfbClientLog("Received TextChat Close\n");
              if (client->HandleTextChat!=NULL)
                  client->HandleTextChat(client, (int)rfbTextChatClose, NULL);
              break;
          case rfbTextChatFinished:
              rfbClientLog("Received TextChat Finished\n");
              if (client->HandleTextChat!=NULL)
                  client->HandleTextChat(client, (int)rfbTextChatFinished, NULL);
              break;
          default:
              buffer=malloc(msg.tc.length+1);
              if (!ReadFromRFBServer(client, buffer, msg.tc.length)) {
                  free(buffer);
                  return false;
              }
              /* Null Terminate <just in case> */
              buffer[msg.tc.length]=0;
              rfbClientLog("Received TextChat \"%s\"\n", buffer);
              if (client->HandleTextChat!=NULL)
                  client->HandleTextChat(client, (int)msg.tc.length, buffer);
              free(buffer);
              break;
          }
      }
      break;
  case rfbXvp: {
          if (!ReadFromRFBServer(client, ((char *)&msg) + 1, sz_rfbXvpMsg -1))
              return false;
          SetClient2Server(client, rfbXvp);
          /* technically, we only care what we can *send* to the server
           * but, we set Server2Client Just in case it ever becomes useful
           */
          SetServer2Client(client, rfbXvp);
          if(client->HandleXvpMsg)
              client->HandleXvpMsg(client, msg.xvp.version, msg.xvp.code);
      }
      break;
  case rfbResizeFrameBuffer: {
          if (!ReadFromRFBServer(client, ((char *)&msg) + 1, sz_rfbResizeFrameBufferMsg -1))
              return false;
          client->width = rfbClientSwap16IfLE(msg.rsfb.framebufferWidth);
          client->height = rfbClientSwap16IfLE(msg.rsfb.framebufferHeigth);
          client->updateRect.x = client->updateRect.y = 0;
          client->updateRect.w = client->width;
          client->updateRect.h = client->height;
          if (!client->MallocFrameBuffer(client))
              return false;
          SendFramebufferUpdateRequest(client, 0, 0, client->width, client->height, false);
          rfbClientLog("Got new framebuffer size: %dx%d\n", client->width, client->height);
      }
      break;
  case rfbPalmVNCReSizeFrameBuffer: {
          if (!ReadFromRFBServer(client, ((char *)&msg) + 1, sz_rfbPalmVNCReSizeFrameBufferMsg -1))
              return false;
          client->width = rfbClientSwap16IfLE(msg.prsfb.buffer_w);
          client->height = rfbClientSwap16IfLE(msg.prsfb.buffer_h);
          client->updateRect.x = client->updateRect.y = 0;
          client->updateRect.w = client->width;
          client->updateRect.h = client->height;
          if (!client->MallocFrameBuffer(client))
              return false;
          SendFramebufferUpdateRequest(client, 0, 0, client->width, client->height, false);
          rfbClientLog("Got new framebuffer size: %dx%d\n", client->width, client->height);
      }
      break;
  default: {
          rfbBool handled = false;
          rfbClientProtocolExtension* e;
          for(e = rfbClientExtensions; !handled && e; e = e->next)
              if(e->handleMessage && e->handleMessage(client, &msg))
                  handled = true;
          if(!handled) {
              char buffer[256];
              rfbClientLog("Unknown message type %d from VNC server\n",msg.type);
              ReadFromRFBServer(client, buffer, 256);
              return false;
          }
      }
      break;
  }
  return true;
}

#define GET_PIXEL8(pix, ptr) ((pix) = *(ptr)++)

#define GET_PIXEL16(pix, ptr) (((uint8_t*)&(pix))[0] = *(ptr)++, \
			       ((uint8_t*)&(pix))[1] = *(ptr)++)

#define GET_PIXEL32(pix, ptr) (((uint8_t*)&(pix))[0] = *(ptr)++, \
			       ((uint8_t*)&(pix))[1] = *(ptr)++, \
			       ((uint8_t*)&(pix))[2] = *(ptr)++, \
			       ((uint8_t*)&(pix))[3] = *(ptr)++)

/* CONCAT2 concatenates its two arguments.  CONCAT2E does the same but also
   expands its arguments if they are macros */

#define CONCAT2(a,b) a##b
#define CONCAT2E(a,b) CONCAT2(a,b)
#define CONCAT3(a,b,c) a##b##c
#define CONCAT3E(a,b,c) CONCAT3(a,b,c)

#define BPP 8
#include "rre.c"
#include "corre.c"
#include "hextile.c"
#include "ultra.c"
#include "zlib.c"
#include "tight.c"
#include "zrle.c"
#undef BPP
#define BPP 16
#include "rre.c"
#include "corre.c"
#include "hextile.c"
#include "ultra.c"
#include "zlib.c"
#include "tight.c"
#include "zrle.c"
#define REALBPP 15
#include "zrle.c"
#undef BPP
#define BPP 32
#include "rre.c"
#include "corre.c"
#include "hextile.c"
#include "ultra.c"
#include "zlib.c"
#include "tight.c"
#include "zrle.c"
#define REALBPP 24
#include "zrle.c"
#define REALBPP 24
#define UNCOMP 8
#include "zrle.c"
#define REALBPP 24
#define UNCOMP -8
#include "zrle.c"
#undef BPP
#include "h264.c"

/*
 * PrintPixelFormat.
 */

void PrintPixelFormat(rfbPixelFormat *format)
{
  if (format->bitsPerPixel == 1) {
    rfbClientLog("  Single bit per pixel.\n");
    rfbClientLog(
	    "  %s significant bit in each byte is leftmost on the screen.\n",
	    (format->bigEndian ? "Most" : "Least"));
  } else {
    rfbClientLog("  %d bits per pixel.\n",format->bitsPerPixel);
    if (format->bitsPerPixel != 8) {
      rfbClientLog("  %s significant byte first in each pixel.\n",
	      (format->bigEndian ? "Most" : "Least"));
    }
    if (format->trueColour) {
      rfbClientLog("  true colour: max red %d green %d blue %d"
		   ", shift red %d green %d blue %d\n",
		   format->redMax, format->greenMax, format->blueMax,
		   format->redShift, format->greenShift, format->blueShift);
    } else {
      rfbClientLog("  Colour map (not true colour).\n");
    }
  }
}

/* avoid name clashes with LibVNCServer */

#define rfbEncryptBytes rfbClientEncryptBytes
#define rfbEncryptBytes2 rfbClientEncryptBytes2
#define rfbDes rfbClientDes
#define rfbDesKey rfbClientDesKey
#define rfbUseKey rfbClientUseKey
#define rfbCPKey rfbClientCPKey

#include "vncauth.c"
#include "d3des.c"
