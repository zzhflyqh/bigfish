/***************************************************************************
 * 
 * Copyright (c) 2012 4399.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file test.cc
 * @author zhangzhihua(zhangzhihua@4399.com)
 * @date 2012/05/02 10:43:53
 * @brief 
 *  
 **/

#include "WorldPacket.h"

enum OPCODE{
    PT_ENTER_SCENE = 1,
    ST_ENTER_SCENE
};

int main( void )
{
    WorldPacket pack( PT_ENTER_SCENE );
    pack << uint32_t(123);
    pack << std::string("zhangzhihua");
    
    uint32_t id;
    std::string name;
    pack >> id;
    pack >> name;

    fprintf( stdout, "%d === %s", id, name.c_str( ) );
    return 0;
}





















/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */