#include <QCoreApplication>
#include <QProcessEnvironment>
#include <QDebug>
#include <QDir>

#include <QDebug>
#include <lua.hpp>
#include <QSettings>
#include <QProcessEnvironment>
#include <QDir>
#include <QFile>
#include "vcard.h"
#include <QList>

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);


    lua_State* L = luaL_newstate();             /* opens Lua */
    luaL_openlibs(L);        /* opens the standard libraries */

    int error = luaL_loadstring(L, "contacts = require('contacts')") || lua_pcall(L, 0, 0, 0);
    if (error) {
        qDebug() << "Error loading contacts: " << QString::fromUtf8(lua_tolstring(L, -1, NULL));
        return 0;
    }

    lua_getglobal(L, "contacts");

#ifdef Q_OS_WIN32
    QString homePath = QProcessEnvironment::systemEnvironment().value("USERPROFILE");
#else
    QString homePath = QProcessEnvironment::systemEnvironment().value("HOME");
#endif

    QDir homeDir = QDir(homePath);
    QString vcf = homeDir.absoluteFilePath(".android/contacts.vcf");
    QFile vcfFile(vcf);
    if (!vcfFile.exists()) {
        vcf = "test.vcf";
    }

    lua_getfield(L, -1, "read_vcf");
    lua_pushstring(L, qPrintable(vcf));

    error = lua_pcall(L, 1, 1, 0);
    if (error) {
        qDebug() << "Error: can't read vcf file: " << vcf << ": " << QString::fromUtf8(lua_tolstring(L, -1, NULL));
        return 0;
    }

    int n = luaL_len(L, -1);
    qDebug() << "n is " << n;

    QList<VCard> mVcards;
    for (int i = 1; i <= n; i++){
        qDebug() << i;
        VCard vcard;
        qDebug() << __LINE__;
        lua_rawgeti(L, -1, i);

        qDebug() << __LINE__ << lua_gettop(L);

        lua_getfield(L, -1, "FN");
        qDebug() << __LINE__;
        vcard.mName = (QString::fromUtf8(lua_tolstring(L, -1, NULL)));
        qDebug() << "name is " << vcard.mName;
        lua_settop(L, -2);

        lua_getfield(L, -1, "TELS");

        int nTel = luaL_len(L, -1);
        for (int iTel = 1; iTel <= nTel; iTel++) {
            lua_rawgeti(L, -1, iTel);
            vcard.mTels.push_back(QString::fromUtf8(lua_tolstring(L, -1, NULL)));
            qDebug() << "tel is " << vcard.mTels;
            lua_settop(L, -2);
        }
        lua_settop(L, -2);

        lua_getfield(L, -1, "EMAILS");
        int nEmail = luaL_len(L, -1);
        for (int iEmail = 1; iEmail <= nEmail; iEmail++) {
            lua_rawgeti(L, -1, iEmail);
            vcard.mEmails.push_back(QString::fromUtf8(lua_tolstring(L, -1, NULL)));
            qDebug() << "email is " << vcard.mEmails;
            lua_settop(L, -2);
        }
        lua_settop(L, -2);

        lua_settop(L, -2);

        mVcards.push_back(vcard);
    }
    lua_settop(L, 0);

    return 0;

    return a.exec();
}
