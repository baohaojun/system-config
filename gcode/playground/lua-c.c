#include <stdio.h>
#include <string.h>
#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"
int main (void) {
    char buff[256];
    int error;
    lua_State *L = luaL_newstate();             /* opens Lua */
    luaL_openlibs(L);        /* opens the standard libraries */
    printf("hello top is %d\n", lua_gettop(L));
    lua_getglobal(L, "require");
    lua_pushstring(L, "t1wrench");
    lua_pcall(L, 1, 1, 0);

    lua_getfield(L, -1, "t1_post");
    lua_pushstring(L, "hello world");
    lua_pcall(L, 1, 1, 0);
    printf("hello top is %d\n", lua_gettop(L));
    lua_close(L);
    return 0;
}
