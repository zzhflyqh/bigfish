/******************************************************* 
    Binding the C++ class to lua,
    get more infomation to visit: 
    http://lua-users.org/wiki/CppBindingWithLunar

    work for Lua5.1.Source from Lua-users.org.
 *******************************************************/

#ifndef __LUNAR_H__
#define __LUNAR_H__
extern "C"
{
#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
}

#include <stdio.h>
#include <string.h>

template <typename T> class Lunar {
public:
    typedef struct { T *pT; } userdataType;
    typedef int (T::*mfp)(lua_State *L);
    typedef struct { const char *name; mfp mfunc; } RegType;

    static void RegisterFunctions(lua_State *L)
    {
        luaL_openlib(L, T::className, T::functions, 0);
        lua_pop(L, 1);
    }

    static void Register(lua_State *L, const char* baseName = NULL) {
        lua_pushstring( L, T::className );
        lua_gettable( L, LUA_GLOBALSINDEX );
        if( lua_isnil(L, -1) )
        {
            lua_pop(L, 1);
            lua_newtable(L);
            lua_pushstring(L, T::className);
            lua_pushvalue(L, -2);
            // store method table in globals so that
            // scripts can add functions written in Lua.
            lua_settable(L, LUA_GLOBALSINDEX);
        }
        int methods = lua_gettop(L);

        luaL_newmetatable(L, T::className);
        int metatable = lua_gettop(L);

        // hide metatable from Lua getmetatable()
        lua_pushvalue(L, methods);
        set(L, metatable, "__metatable");

        lua_pushvalue(L, methods);
        set(L, metatable, "__index");

        lua_pushcfunction(L, tostring_T);
        set(L, metatable, "__tostring");

        lua_pushcfunction(L, gc_T);
        set(L, metatable, "__gc");
        
        lua_newtable(L);                // mt for method table
        lua_pushcfunction(L, new_T);
        lua_pushvalue(L, -1);           // dup new_T function
        set(L, methods, "c_new");       // add new_T to method table
        set(L, -3, "__call");           // mt.__call = new_T

        if( baseName ){
            lua_pushstring( L, baseName );
            lua_gettable( L, LUA_GLOBALSINDEX );    // get base_table
            if( lua_isnil(L, -1) )
            {
                lua_pop(L, 1);
                lua_newtable(L);
                lua_pushstring(L, baseName);
                lua_pushvalue(L, -2);
                lua_settable(L, LUA_GLOBALSINDEX);
            }
            set(L, -3, "__index");      // mt.__index = base_table
        }

        lua_setmetatable(L, methods);

        // fill method table with methods from class T
        for (RegType *l = T::methods; l->name; l++) {
            lua_pushstring(L, l->name);
            lua_pushlightuserdata(L, (void*)l);
            lua_pushcclosure(L, thunk, 1);
            lua_settable(L, methods);
        }

        lua_pop(L, 2);  // drop metatable and method table
    }

    /*! call named lua method from userdata method table */
    static int call_method(lua_State *L, const char *method,
        int nargs=0, int nresults=LUA_MULTRET, int errfunc=0)
    {
        int base = lua_gettop(L) - nargs;  // userdata index
        if (!luaL_checkudata(L, base, T::className)) {
            lua_settop(L, base-1);           // drop userdata and args
            lua_pushfstring(L, "not a valid %s userdata", T::className);
            return -1;
        }

        lua_pushstring(L, method);         // method name
        lua_gettable(L, base);             // get method from userdata
        if (lua_isnil(L, -1)) {            // no method?
            lua_settop(L, base-1);         // drop userdata and args
            lua_pushfstring(L, "%s missing method '%s'", T::className, method);
            return -1;
        }
        lua_insert(L, base);               // put method under userdata, args

        int status = lua_pcall(L, 1+nargs, nresults, errfunc);  // call method
        if (status) {
            const char *msg = lua_tostring(L, -1);
            if (msg == NULL) msg = "(error with no message)";
            lua_pushfstring(L, "%s:%s status = %d\n%s",
                T::className, method, status, msg);
            lua_remove(L, base);           // remove old message
            return -1;
        }
        return lua_gettop(L) - base + 1;   // number of results
    }

    /*! push onto the Lua stack a userdata containing a pointer to T object */
    static int push(lua_State *L, T *obj, bool gc=false) {
        if (!obj) { lua_pushnil(L); return 0; }
        luaL_getmetatable(L, T::className);  // lookup metatable in Lua registry
        if (lua_isnil(L, -1)) luaL_error( L, "%s missing metatable", T::className );
        int mt = lua_gettop(L);
        subtable(L, mt, "userdata", "v");   
        userdataType *ud =
            static_cast<userdataType*>(pushuserdata(L, obj, sizeof(userdataType)));
        if (ud) {
            ud->pT = obj;  // store pointer to object in userdata
            lua_pushvalue(L, mt);
            lua_setmetatable(L, -2);
            if (gc == false) {
                lua_checkstack(L, 3);
                subtable(L, mt, "do not trash", "k");   
                lua_pushvalue(L, -2);
                lua_pushboolean(L, 1);
                lua_settable(L, -3);
                lua_pop(L, 1);
            }
        }
        lua_replace(L, mt);
        lua_settop(L, mt);
        return mt;  // index of userdata containing pointer to T object
    }

    /*! get userdata from Lua stack and return pointer to T object */
	static T *check(lua_State *L, int narg) {
		userdataType *ud = static_cast<userdataType*>(luaL_checkudata(L, narg, T::className));
		if(!ud)
		{
			luaL_typerror(L, narg, T::className);
			return NULL;
		}
		return ud->pT;  // pointer to T object
	}

	static T *nocheck(lua_State *L, int narg ) {
		userdataType* ud = static_cast<userdataType*>(lua_touserdata(L, narg));
		return ud ? ud->pT : NULL;
	}
private:
    Lunar();  // hide default constructor

    /*! member function dispatcher */
    static int thunk(lua_State *L) {
        // stack has userdata, followed by method args
        T *obj = check(L, 1);  // get 'self', or if you prefer, 'this'
        lua_remove(L, 1);  // remove self so member function args start at index 1
        // get member function from upvalue
        RegType *l = static_cast<RegType*>(lua_touserdata(L, lua_upvalueindex(1)));
        if( !obj || !l )
        {
        	luaL_error( L, "Luar::calling unknown function of %s", T::className );
        }
        return (obj->*(l->mfunc))(L);  // call member function
    }

    /*! garbage collection metamethod */
    static int gc_T(lua_State *L) {
        if (luaL_getmetafield(L, 1, "do not trash"))
        {
            lua_pushvalue(L, 1);  // dup userdata
            lua_gettable(L, -2);
            if (!lua_isnil(L, -1)) return 0;  // do not delete object
        }
        if( T::gc_delete_ )
        {
        	userdataType *ud = static_cast<userdataType*>(lua_touserdata(L, 1));
        	T *obj = ud->pT;
        	if (obj) delete obj;  // call destructor for T objects
        }
        return 0;
    }

    // create a new T object and
    // push onto the Lua stack a userdata containing a pointer to T object
    static int new_T(lua_State *L) {
        lua_remove(L, 1);   // use classname:new(), instead of classname.new()
        T *obj = new T();  // call constructor for T objects
        push(L, obj, true); // gc_T will delete this object
        return 1;           // userdata containing pointer to T object
    }

    static int tostring_T (lua_State *L) {
        char buff[32];
        userdataType *ud = static_cast<userdataType*>(lua_touserdata(L, 1));
        T *obj = ud->pT;
        sprintf(buff, "%p", obj);
        lua_pushfstring(L, "%s (%s)", T::className, buff);
        return 1;
    }

    static void set(lua_State *L, int table_index, const char *key) {
        lua_pushstring(L, key);
        lua_insert(L, -2);  // swap value and key
        lua_settable(L, table_index);
    }

    static void weaktable(lua_State *L, const char *mode) {
        lua_newtable(L);
        lua_pushvalue(L, -1);  // table is its own metatable
        lua_setmetatable(L, -2);
        lua_pushliteral(L, "__mode");
        lua_pushstring(L, mode);
        lua_settable(L, -3);   // metatable.__mode = mode
    }

    static void subtable(lua_State *L, int tindex, const char *name, const char *mode) {
        lua_pushstring(L, name);
        lua_gettable(L, tindex);
        if (lua_isnil(L, -1)) {
            lua_pop(L, 1);
            lua_checkstack(L, 3);
            weaktable(L, mode);
            lua_pushstring(L, name);
            lua_pushvalue(L, -2);
            lua_settable(L, tindex);
        }
    }

    static void *pushuserdata(lua_State *L, void *key, size_t sz) {
        void *ud = 0;
        lua_pushlightuserdata(L, key);
        lua_gettable(L, -2);     // lookup[key]
        if (lua_isnil(L, -1)) {
            lua_pop(L, 1);         // drop nil
            lua_checkstack(L, 3);
            ud = lua_newuserdata(L, sz);  // create new userdata
            lua_pushlightuserdata(L, key);
            lua_pushvalue(L, -2);  // dup userdata
            lua_settable(L, -4);   // lookup[key] = userdata
        }
        return ud;
    }
};
    
    
namespace Lua
{    
    template <typename T>
    inline void GetTableNumber( lua_State* L, int nTableIndex, const char* szKey, T& nDstNumber );
    template <typename T>
    inline void GetTableNumberByIndex( lua_State* L, int nTableIndex, int nIndex, T& fDstNumber );
    void GetTableString( lua_State* L, int nTableIndex, const char* szKey, char* szDstString, int nDstLen );
    void GetTableStringByIndex( lua_State* L, int nTableIndex, int nIndex, char* szDstString, int nDstLen );
    int  GetTable( lua_State* L, int nTableIndex, const char* szKey );
	int  GetTable( lua_State* L, int nTableIndex, int index );
        
    int DoFile( lua_State* L, const char* fileName );
    int DoFile( lua_State* L );
}


template <typename T>
inline void Lua::GetTableNumber( lua_State* L, int nTableIndex, const char* szKey, T& nDstNumber )
{
    lua_pushvalue( L, nTableIndex );
    lua_pushstring( L, szKey );
    lua_gettable( L, -2 );
    if( !lua_isnil(L, -1) ){
        nDstNumber = (T)lua_tonumber(L, -1);
	}
    lua_pop( L, 2 );
}


template <typename T>
inline void Lua::GetTableNumberByIndex( lua_State* L, int nTableIndex, int nIndex, T& fDstNumber )
{
    lua_pushvalue( L, nTableIndex );
    lua_rawgeti( L, -1, nIndex );
    if( !lua_isnil(L, -1) ) {
        fDstNumber = (T)lua_tonumber(L, -1);
	}
    lua_pop( L, 2 );
}


#define WHILE_TABLE(L) lua_pushnil(L); while( lua_next(L, -2) ) {
#define END_WHILE(L) lua_pop(L, 1); }

#define method(class, name) {#name, &class::name}
#define function(class, name) {#name, class::name}
#define Class(class) #class

#endif  //_LUNAR_H_
