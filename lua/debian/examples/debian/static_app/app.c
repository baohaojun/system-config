#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

int main(void)
{
    int result;
    lua_State* L = lua_open();
    luaL_openlibs(L);
    result = luaL_dostring(L, "print 'hello from Lua'");
    lua_close(L);
    return (result != 0);
}
