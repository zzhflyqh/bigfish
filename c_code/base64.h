/***************************************************************************
 * 
 * Copyright (c) 2011 4399.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file base64.h
 * @author zhangzhihua(zhangzhihua@4399.com)
 * @date 2011/11/11 19:11:16
 * @brief 
 *  
 **/



#ifndef  __BASE64_H_
#define  __BASE64_H_


extern void base64_decode(const char *data, int input_length, char *decoded_data, int *output_length); 

extern void base64_encode(const char *in_str, int in_len, char *out_str);




#endif  //__BASE64_H_

/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
