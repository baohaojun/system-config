
enum desc_type {
	UINT64,
	SINT64,
	UINT32,
	SINT32,
	UINT16,
	SINT16,
	UINT8,
	SINT8,
	FOURCC,
	STRING,
	PTR,
	ENUM16,
	ENUM32,
	STRUCT,
	UNION,
	BITS16,
	BITS32,
	BITS64,
	VER,
	PADDING,
};

struct struct_desc {
	enum desc_type      type;
	char                *name;
	unsigned int        length;
	char                **enums;
	char                **bits;
	struct struct_desc  *desc;
	struct {
		unsigned int        value;
		char                *name;
		struct struct_desc  *desc;
	} u[16];
};

struct ioctl_desc {
	char                *name;
	struct struct_desc  *desc;
};

/* ---------------------------------------------------------------------- */

extern struct struct_desc desc_int[];
extern struct struct_desc desc_long[];
extern struct struct_desc desc_timeval[];

/* ---------------------------------------------------------------------- */

int print_struct(FILE *fp, struct struct_desc *desc, void *data,
		 char *prefix, int tab);
int print_ioctl(FILE *fp, struct ioctl_desc *ioctls, char *prefix,
		int cmd, void *ptr);
