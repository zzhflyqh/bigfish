/***************************************************************************
 * 
 * Copyright (c) 2012 4399.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file luapack.h
 * @author zhangzhihua(zhangzhihua@4399.com)
 * @date 2012/05/02 16:31:20
 * @brief 
 *  
 **/




#ifndef  __LUAPACK_H_
#define  __LUAPACK_H_


#include "lunar.h"
#include "ByteBuffer.h"


class LuaPack{
    public:
        static const char className[];
        static Lunar<LuaPack>::RegType methods[];

        ByteBuffer *buffer_;

    public:
        LuaPack( );
        LuaPack( ByteBuffer *pBuffer );
        //write data to ar                                                                                                                        
        int write_ubyte( lua_State* L );//无符号数                                                                                                
        int write_char( lua_State* L ); //有符号数                                                                                                
        int write_short( lua_State* L );                                                                                                          
        int write_ushort( lua_State* L );                                                                                                         
        int write_int( lua_State* L );                                                                                                            
        int write_uint( lua_State* L );                                                                                                           
        int write_float( lua_State* L );                                                                                                          
        int write_double( lua_State* L );                                                                                                         
        int write_string( lua_State* L );                                                                                                         
        int write_boolean( lua_State* L );                                                                                                        
        int write_buffer( lua_State* L );                                                                                                         
        int write_format( lua_State* L );                                                                                                         
                                                                                                                                                  
        //read data from ar                                                                                                                       
        int read_ubyte( lua_State* _L ); //无符号数                                                                                               
        int read_char( lua_State* _L );  //有符号数                                                                                               
        int read_short( lua_State* _L );                                                                                                          
        int read_ushort( lua_State* _L );                                                                                                         
        int read_int( lua_State* _L );                                                                                                            
        int read_uint( lua_State* _L );                                                                                                           
        int read_float( lua_State* _L );                                                                                                          
        int read_double( lua_State* _L );                                                                                                         
        int read_string( lua_State* _L );   
        int read_boolean( lua_State* _L );                                                                                                        
                                                                                                                                                  
        int is_loading( lua_State* _L );                                                                                                          
        int is_storing( lua_State* _L );                                                                                                          
                                                                                                                                                  
        int get_offset( lua_State* _L );                                                                                                          
        int write_int_at( lua_State* _L );                                                                                                        
        int write_byte_at( lua_State* _L );                                                                                                       
                                                                                                                                                  
        int write_sntype( lua_State* _L );                                                                                                        
                                                                                                                                                  
        int c_delete( lua_State* _L );                                                                                                            
                                                                                                                                                  
        int bpack( lua_State* _L );                                                                                                               
        int upack( lua_State* _L );       

        int get_buffer( lua_State* _L );   
        int flush( lua_State* _L ); 
};




#define LUNAR_METHODS_LIST  \
    method( LuaPack, before_send ) \
    method( LuaPack, write_ubyte ) \
    method( LuaPack, write_char ) \
    method( LuaPack, write_short ) \
    method( LuaPack, write_ushort ) \
    method( LuaPack, write_int ) \
    method( LuaPack, write_uint ) \
    method( LuaPack, write_float ) \
    method( LuaPack, write_double ) \
    method( LuaPack, write_string ) \
    method( LuaPack, write_boolean ) \
    method( LuaPack, write_buffer ) \
    method( LuaPack, write_format ) \
    method( LuaPack, write_format ) \
\
    method( LuaPack, read_ubyte ) \
    method( LuaPack, read_char ) \
    method( LuaPack, read_short ) \
    method( LuaPack, read_ushort ) \
    method( LuaPack, read_int ) \
    method( LuaPack, read_uint ) \
    method( LuaPack, read_float ) \
    method( LuaPack, read_double ) \
    method( LuaPack, read_string ) \
    method( LuaPack, read_boolean ) \
\
    method( LuaPack, is_loading ) \
    method( LuaPack, is_storing ) \
    method( LuaPack, get_offset ) \
    method( LuaPack, write_int_at ) \
    method( LuaPack, write_byte_at ) \
    method( LuaPack, write_sntype ) \
    method( LuaPack, c_delete ) \
    method( LuaPack, bpack ) \
    method( LuaPack, upack ) \
    method( LuaPack, get_buffer ) \
    method( LuaPack, flush )\










#endif  //__LUAPACK_H_

/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
