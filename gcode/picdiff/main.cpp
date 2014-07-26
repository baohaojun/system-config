#include <QCoreApplication>
#include <QtGui/QImage>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main(int argc, char *argv[])
{
    QImage a(argv[1]);
    QImage b(argv[2]);

    uchar* aBits = a.bits();
    uchar* aBuffer = (uchar*)malloc(a.byteCount());
    memcpy(aBuffer, aBits, a.byteCount());

    uchar* bBits = b.bits();
    uchar* bBuffer = (uchar*)malloc(b.byteCount());
    memcpy(bBuffer, bBits, b.byteCount());

    double diff = 0;
    int byteCount = (int)fmin(a.byteCount(), b.byteCount());
    printf("byteCount is %d\n", byteCount);
    for (int i = 0; i < byteCount; i++) {
        diff += abs(bBuffer[i] - aBuffer[i]);
    }
    diff /= byteCount;

    printf("diff is %f, byteCount is %d\n", diff, byteCount);
}
