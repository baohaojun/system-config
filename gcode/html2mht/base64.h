#pragma once

int ToBase64Length(int inputlen);
int ToBase64(unsigned char *out, const unsigned char *in, int inlen);
int FromBase64(unsigned char *out, const unsigned char *in);