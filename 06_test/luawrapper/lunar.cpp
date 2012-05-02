#include "lunar.h"

void Lua::GetTableStringByIndex( lua_State* L, int nTableIndex, int nIndex, char* szDstString, int nDstLen )
{
    lua_pushvalue( L, nTableIndex );
	lua_rawgeti( L, -1, nIndex );	
	if( !lua_isnil(L, -1) ) {
		strlcpy( szDstString, lua_tostring(L, -1), nDstLen );
	}
	lua_pop( L, 2 );
}


int Lua::GetTable( lua_State* L, int nTableIndex, const char* szKey )
{
    lua_pushvalue( L, nTableIndex );    
	lua_pushstring( L, szKey );
	lua_gettable( L, -2 );

	if( lua_isnil(L, -1) || !lua_istable(L, -1) )
	{
		lua_pop(L, 2);
		return -1;
	}
	else
	{
	    lua_insert(L, -2);
	    lua_pop(L, 1);
		return lua_gettop(L);
	}
}

int Lua::GetTable( lua_State* L, int nTableIndex, int index )
{
	lua_pushvalue( L, nTableIndex );
	lua_pushnumber( L, index );
	lua_gettable( L, -2 );
	if( lua_isnil(L, -1) || !lua_istable(L, -1) )
	{
		lua_pop(L, 2);
		return -1;
	}
	else
	{
	    lua_insert(L, -2);
	    lua_pop(L, 1);
		return lua_gettop(L);
	}
}


/*! Do the script file in package.
    \return 0 for success
    */
int Lua::DoFile( lua_State* L, const char* fileName )
{
	if( luaL_dofile(L, fileName) ) {
		return 1;
	}

	return 0;
}


/*! Wrap to Lua
    */
int Lua::DoFile( lua_State* L )
{
	const char* fileName = lua_tostring(L, -1);

	int ret = DoFile(L, fileName);

	lua_pushnumber(L, (lua_Number)ret);
	return 0;
}

