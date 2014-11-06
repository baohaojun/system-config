#include <stdio.h>
#include <string.h>
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"
int main (void) {

    lua_State *L = luaL_newstate();             /* opens Lua */
    luaL_openlibs(L);        /* opens the standard libraries */
    fprintf(stderr, "hello world %s %s %d\n", __FILE__, __FUNCTION__, __LINE__);
    int error = luaL_loadstring(L, "t1wrench = require('t1wrench')") || lua_pcall(L, 0, 0, 0);
    if (error) {
        printf("exit %s\n", "Can't load t1wrench");
        lua_close(L);
        return;
    }

    printf("hello top is %d\n", lua_gettop(L));
    lua_getglobal(L, "t1wrench");
    printf("hello top is %d\n", lua_gettop(L));
    while (1) {
        const char *func = "t1_post";
        lua_getfield(L, -1, (func));
        printf("hello top is %d\n", lua_gettop(L));
        lua_pushstring(L, "hello world");
        printf("hello top is %d\n", lua_gettop(L));
        fprintf(stderr, "Press return to continue... ");
        fflush(stderr);
        fgetc(stdin);

        error = lua_pcall(L, 1, 1, 0);
        if (error) {
            printf("Exit %s\n", lua_tolstring(L, -1, NULL));
            lua_close(L);
            return -1;
        }
        printf("hello top is %d\n", lua_gettop(L));
        lua_pop(L, 1);
    }
    return 0;
}
