#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <ctype.h>
#include <sys/ioctl.h>

#include "struct-dump.h"

/* ---------------------------------------------------------------------- */

struct struct_desc desc_int[] = {{
	.type   = SINT32,
	.name   = "int",
},{
	/* end of list */
}};

struct struct_desc desc_long[] = {{
	.type   = SINT32,
	.name   = "long",
},{
	/* end of list */
}};

struct struct_desc desc_timeval[] = {{
	/* FIXME */
	/* end of list */
}};

/* ---------------------------------------------------------------------- */

int print_struct(FILE *fp, struct struct_desc *desc, void *data,
		 char *prefix, int tab)
{
	char name[256];
	unsigned char *ptr = data;
	uint64_t u64;
	int64_t  s64;
	uint32_t u32;
	int32_t  s32;
	uint16_t u16;
	int16_t  s16;
	uint8_t  u8;
	int8_t   s8;
	struct al64_t { char c; uint64_t t; } al64_t;
	int al = sizeof(long)-1; /* struct + union */
	int al64 = (unsigned long)&al64_t.t - (unsigned long)&al64_t.c - 1; /* 64 bit alignement */
	void *p;
	unsigned int i,j,first;

	for (i = 0; desc[i].name != NULL; i++) {
		sprintf(name,"%s%s",prefix,desc[i].name);
		if (STRUCT == desc[i].type) {
			strcat(name,".");
			ptr = (void*)(((intptr_t)ptr + al) & ~al);
			print_struct(fp,desc[i].desc, ptr, name, tab);
			ptr += desc[i].length;
			if (!tab && desc[i+1].name != NULL)
				fprintf(fp,";");
			continue;
		}
		if (UNION == desc[i].type) {
			u32 = *((uint32_t*)(ptr-4));
			ptr = (void*)(((intptr_t)ptr + al) & ~al);
			for (j = 0; desc[i].u[j].name != NULL; j++)
				if (desc[i].u[j].value == u32)
					break;
			if (desc[i].u[j].name != NULL) {
				strcat(name,".");
				strcat(name,desc[i].u[j].name);
				strcat(name,".");
				print_struct(fp,desc[i].u[j].desc,
					     ptr, name, tab);
			}
			return 0; /* FIXME */
		}
		if (tab)
			fprintf(fp,"\t%-24s: ",name);
		else
			fprintf(fp,"%s=",name);
		switch (desc[i].type) {
		case STRING:
			fprintf(fp,"\"%-.*s\"",desc[i].length,ptr);
			ptr += desc[i].length;
			break;
		case PTR:
			p = *(void**)ptr;
			fprintf(fp,"%p",p);
			ptr += sizeof(p);
			break;
		case VER:
			u32 = *((uint32_t*)ptr);
			fprintf(fp,"%d.%d.%d",
				(u32 >> 16) & 0xff,
				(u32 >>  8) & 0xff,
				u32         & 0xff);
			ptr += 4;
			break;
		case FOURCC:
			u32 = *((uint32_t*)ptr);
			fprintf(fp,"0x%08x [%c%c%c%c]", u32,
				isprint(ptr[0]) ? ptr[0] : '.',
				isprint(ptr[1]) ? ptr[1] : '.',
				isprint(ptr[2]) ? ptr[2] : '.',
				isprint(ptr[3]) ? ptr[3] : '.');
			ptr += 4;
			break;

		case ENUM16:
			u16 = *((uint16_t*)ptr);
			fprintf(fp,"%s", (u16 < desc[i].length && desc[i].enums[u16])
				? desc[i].enums[u16] : "unknown");
			ptr += 2;
			break;
		case ENUM32:
			u32 = *((uint32_t*)ptr);
			fprintf(fp,"%s", (u32 < desc[i].length && desc[i].enums[u32])
				? desc[i].enums[u32] : "unknown");
			ptr += 4;
			break;

		case BITS16:
			u16 = *((uint16_t*)ptr);
			first = 1;
			fprintf(fp,"0x%x [",u16);
			for (j = 0; j < 16; j++) {
				if (0 == (u16 & (1 << j)))
					continue;
				fprintf(fp,"%s%s",
					first ? "" : ",",
					desc[i].bits[j]);
				first = 0;
			}
			fprintf(fp,"]");
			ptr += 2;
			break;
		case BITS32:
			u32 = *((uint32_t*)ptr);
			first = 1;
			fprintf(fp,"0x%x [",u32);
			for (j = 0; j < 32; j++) {
				if (0 == (u32 & (1 << j)))
					continue;
				fprintf(fp,"%s%s",
					first ? "" : ",",
					desc[i].bits[j]);
				first = 0;
			}
			fprintf(fp,"]");
			ptr += 4;
			break;
		case BITS64:
			ptr = (void*)(((intptr_t)ptr + al64) & ~al64);
			u64 = *((uint64_t*)ptr);
			first = 1;
			fprintf(fp,"0x%" PRIx64 " [",u64);
			for (j = 0; j < 64; j++) {
				if (0 == (u64 & ((int64_t)1 << j)))
					continue;
				fprintf(fp,"%s%s",
					first ? "" : ",",
					desc[i].bits[j]);
				first = 0;
			}
			fprintf(fp,"]");
			ptr += 8;
			break;

		case UINT64:
			ptr = (void*)(((intptr_t)ptr + al64) & ~al64);
			u64 = *((uint64_t*)ptr);
			fprintf(fp,"%" PRIu64,u64);
			ptr += 8;
			break;
		case SINT64:
			ptr = (void*)(((intptr_t)ptr + al64) & ~al64);
			s64 = *((int64_t*)ptr);
			fprintf(fp,"%" PRId64,s64);
			ptr += 8;
			break;
		case UINT32:
			u32 = *((uint32_t*)ptr);
			fprintf(fp,"%u",u32);
			ptr += 4;
			break;
		case SINT32:
			s32 = *((int32_t*)ptr);
			fprintf(fp,"%d",s32);
			ptr += 4;
			break;
		case UINT16:
			u16 = *((uint16_t*)ptr);
			fprintf(fp,"%u",u16);
			ptr += 2;
			break;
		case SINT16:
			s16 = *((int16_t*)ptr);
			fprintf(fp,"%d",s16);
			ptr += 2;
			break;
		case UINT8:
			u8 = *((uint8_t*)ptr);
			fprintf(fp,"%u",u8);
			ptr += 1;
			break;
		case SINT8:
			s8 = *((int8_t*)ptr);
			fprintf(fp,"%d",s8);
			ptr += 1;
			break;

		case PADDING:
			ptr += desc[i].length;
			break;

		case STRUCT:
		case UNION:
			/* shouldn't happen */
			fprintf(fp,"FIXME [type=%d]\n",desc[i].type);
			exit(1);
		}
		if  (tab)
			fprintf(fp,"\n");
		else if (desc[i+1].name != NULL)
			fprintf(fp,";");
	}
	return 0;
}

/* ---------------------------------------------------------------------- */

int print_ioctl(FILE *fp, struct ioctl_desc *ioctls, char *prefix,
		int cmd, void *ptr)
{
	int  index               = _IOC_NR(cmd);
	char *name               = ioctls[index].name;
	struct struct_desc *desc = ioctls[index].desc;

	fprintf(fp,"%s%s(", prefix, name ? name : "UNKNOWN");
	if (desc) {
		print_struct(fp,desc,ptr,"",0);
	} else {
		fprintf(stderr,"???");
	}
	fprintf(fp,")");
	return 0;
}

/* ---------------------------------------------------------------------- */
/*
 * Local variables:
 * c-basic-offset: 8
 * End:
 */
