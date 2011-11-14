/***************************************************************************
 * 
 * Copyright (c) 2011 4399.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file base64.cpp
 * @author zhangzhihua(zhangzhihua@4399.com)
 * @date 2011/11/11 19:17:11
 * @brief 
 *  
 **/

#include "base64.h"
#include <string.h>


typedef unsigned int uint32_t;

static char map64[] = { 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63, 
	52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, 0, -1, -1, 
	-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 
	17, 18, 19, 20, 21, 22, 23, 24, 25, -1, -1, -1, -1, -1, -1, 26, 
	27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 
	43, 44, 45, 46, 47, 48, 49, 50, 51, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1,-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
	-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};

static char alphabet64[] = { 
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 
	'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 
	'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 
	'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 
	'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 
	'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 
	'w', 'x', 'y', 'z', '0', '1', '2', '3', 
	'4', '5', '6', '7', '8', '9', '+', '/', 
};

/*********************************** Code *************************************/ 
/* 
 * * Decode a buffer from "string" and into "outbuf" 
 * */
void base64_decode(const char *data,int input_length,char *decoded_data, int *output_length) {


    if (input_length % 4 != 0) return ;

    *output_length = input_length / 4 * 3;
    if (data[input_length - 1] == '=') (*output_length)--;
    if (data[input_length - 2] == '=') (*output_length)--;

    if (decoded_data == NULL) return ;

    for (int i = 0, j = 0; i < input_length;) {

        uint32_t sextet_a = data[i] == '=' ? 0 & i++ : map64[data[i++]];
        uint32_t sextet_b = data[i] == '=' ? 0 & i++ : map64[data[i++]];
        uint32_t sextet_c = data[i] == '=' ? 0 & i++ : map64[data[i++]];
        uint32_t sextet_d = data[i] == '=' ? 0 & i++ : map64[data[i++]];

        uint32_t triple = (sextet_a << 3 * 6)
                        + (sextet_b << 2 * 6)
                        + (sextet_c << 1 * 6)
                        + (sextet_d << 0 * 6);

        if (j < *output_length) decoded_data[j++] = (triple >> 2 * 8) & 0xFF;
        if (j < *output_length) decoded_data[j++] = (triple >> 1 * 8) & 0xFF;
        if (j < *output_length) decoded_data[j++] = (triple >> 0 * 8) & 0xFF;
    }
}
			
/******************************************************************************/ 
/* 
* Encode a buffer from "string" into "outbuf" 
*/
void base64_encode(const char *in_str, int in_len, char *out_str)
{ 
    static unsigned char base64[] ="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    int curr_out_len = 0;
    int i = 0;
    unsigned char a, b, c;
    out_str[0] = '\0';
    if (in_len > 0)
    {
       while (i < in_len)
       {
        a = in_str[i];
        b = (i + 1 >= in_len) ? 0 : in_str[i + 1];
        c = (i + 2 >= in_len) ? 0 : in_str[i + 2];
        if (i + 2 < in_len)
        {
         out_str[curr_out_len++] = (base64[(a >> 2) & 0x3F]);
         out_str[curr_out_len++] = (base64[((a << 4) & 0x30) + ((b >> 4) & 0xf)]);
         out_str[curr_out_len++] = (base64[((b << 2) & 0x3c) + ((c >> 6) & 0x3)]);
         out_str[curr_out_len++] = (base64[c & 0x3F]);
        }
        else if (i + 1 < in_len)
        {
         out_str[curr_out_len++] = (base64[(a >> 2) & 0x3F]);
         out_str[curr_out_len++] = (base64[((a << 4) & 0x30) + ((b >> 4) & 0xf)]);
         out_str[curr_out_len++] = (base64[((b << 2) & 0x3c) + ((c >> 6) & 0x3)]);
         out_str[curr_out_len++] = '=';
        }
        else
        {
         out_str[curr_out_len++] = (base64[(a >> 2) & 0x3F]);
         out_str[curr_out_len++] = (base64[((a << 4) & 0x30) + ((b >> 4) & 0xf)]);
         out_str[curr_out_len++] = '=';
         out_str[curr_out_len++] = '=';
        }
        i += 3;
       }
       out_str[curr_out_len] = '\0';
    }
    return;
}


/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
