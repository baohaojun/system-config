#ifndef __BHJDEBUG_H__
#define __BHJDEBUG_H__


static int BHJDEBUG(const char* fmt, ...)
{
	va_list ap;

	CString buf;
	va_start(ap, fmt);
	buf.FormatV(fmt, ap);
	va_end(ap);
#ifdef _DEBUG
	TRACE("%s\n", buf.GetBuffer(0));
#endif
	printf("%s\n", buf.GetBuffer(0));	
	fflush(stdout);
	return 0;
}

static int bhjerr(const char* fmt, ...)
{
	va_list ap;

	CString buf;
	va_start(ap, fmt);
	buf.FormatV(fmt, ap);
	va_end(ap);
#ifdef _DEBUG
	TRACE("%s\n", buf.GetBuffer(0));
#endif
	printf("%s\n", buf.GetBuffer(0));	
	fflush(stdout);
	exit(-1);
}
#endif

