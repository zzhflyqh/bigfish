#include "lua_server.h"

LuaServer::LuaServer( )
{
	int idx = 0;
	for( ; idx < MAX_REFS; ++idx ){
		refs_[ idx ] = LUA_NOREF;
	}
	strlcpy( main_file_, INIT_LUA_MAIN_FILE, MAX_CHARS );
	memset( lua_dir_, 0 , MAX_CHARS );
	L_ = NULL;
}

LuaServer::~LuaServer( )
{
	if( L_ ){
		// do gc and unregister
	}
}

int LuaServer::hook_lua_err( lua_State *_L )
{
	lua_Debug ldb;
	for( int i=0; lua_getstack( _L, i, &ldb ) ==  1; ++i ){
		lua_getinfo( _L, ">Slnu", &ldb );
		const char *name = ldb.name ? ldb.name: "";
		const char *file_name = ldb.source ? ldb.source: "";
		fprintf( stderr, "[LUASERVER] %s '%s' @file '%s', line %d\n", ldb.what, name, file_name, ldb.currentline );
	}
}

int LuaServer::init( const char *_lua_dir );
{
	if( !_lua_dir ){
		return -1;
	}

	L_ = new lua_State( );
	luaL_openlibs( L_ );
	lua_atpanic( L_, hook_lua_err );
}

int LuaServer::register_class( )
{

}

int LuaServer::get_table_field_ref( const char *_table, const char *_field, int _ref, int _index )
{
	lua_getglobal( L_, _table );
	if( lua_isnil( L_, -1 ) ){
		return -1;
	}
	lua_getfield( L_, -1, _field );
	if( lua_isnil( L_, -1 ) ){
		return -1;
	}

	refs_[ _ref ] = luaL_ref( L_, _index );
	lua_pop( L_, 1 );
	return 0;
}


