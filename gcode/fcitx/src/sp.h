#ifndef _SP_H
#define _SP_H

typedef struct _SP_C {
    char            strQP[5];
    char            cJP;
} SP_C;

typedef struct _SP_S {
    char            strQP[3];
    char            cJP;
} SP_S;

extern Bool            bSP_UseSemicolon;
extern Bool            bSP;
extern char            cNonS;

extern SP_C            SPMap_C[];
extern SP_S            SPMap_S[];


void            LoadSPData (void);

//void            QP2SP (char *strQP, char *strSP);
void            SP2QP (char *strSP, char *strQP);
int             GetSPIndexQP_C (char *str);
int             GetSPIndexQP_S (char *str);
int             GetSPIndexJP_C (char c, int iStart);
int             GetSPIndexJP_S (char c);
void            SwitchSP (void);
void 		SPInit (void);

#endif
