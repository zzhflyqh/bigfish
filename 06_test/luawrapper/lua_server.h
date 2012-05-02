extern "C"{
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
}

#define MAX_REFS 1024
#define MAX_CHARS 1024

#ifdef DEBUG
#define INIT_LUA_MAIN_FILE "./main.lua"
#else
#define INIT_LUA_MAIN_FILE "./main.lc"
#endif

enum LuaRef{
	ON_EXIT        = 1,
	ON_FRAME_MOVE
};

class LuaServer{
	public:
		LuaServer( );
		virtual ~LuaServer( );
		int init( const char *_lua_dir );
		void stop( );
		int start( );
	
	private:
		int refs_[ MAX_REFS ]
		int lua_State *L_;
		char lua_dir_[ MAX_CHARS ];
		char main_file_[ MAX_CHARS ];
	
	private:
        int update_lua_refs( );
		int unregister_lua_refs( );
		int register_class( );
		int register_global( );
		int get_table_field_ref( const char *_table, const char *_field, int _ref, int _index );
		int hook_lua_err( lua_State *_L );
};
