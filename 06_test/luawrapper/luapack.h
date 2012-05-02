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
#include "WorldPacket.h"

#define LUNAR_METHODS_LIST  \
    method( LuaPack, before_send ) \

class LuaPack{
    public:
        static const char className[];
        static Lunar<LuaPack>::RegType methods[];

};













#endif  //__LUAPACK_H_

/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
