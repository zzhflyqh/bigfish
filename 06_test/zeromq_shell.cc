/***************************************************************************
 * 
 * Copyright (c) 2012 4399.com, Inc. All Rights Reserved
 * 
 **************************************************************************/
 
 
 
/**
 * @file zeromq_shell.cc
 * @author zhangzhihua(zhangzhihua@4399.com)
 * @date 2012/04/28 16:30:41
 * @brief 
 *  
 **/

#include <stdexcept>
#include <iostream>
using std::cout;
using std::endl;

#include <cstdlib>
#include <cstring>

#include <zmq.hpp>

void usage() {
    cout << "Usage: \n"
         << " zmq <SOCKET_TYPE> <URI> <MSG>\n"
         << "\n";

    std::exit(0);
}

int get_socket_type(const char *str) {
    int retval = 0;

    if (! std::strcmp(str, "REQ")) {
        retval = ZMQ_REQ;
    }
    else {
        throw std::invalid_argument(
            "Invalid socket type. Only \"REQ\" is supported."
        );
    }

    return retval;
}

void recv_multipart(zmq::socket_t &socket) {
    int64_t more=-1;
    size_t more_size = sizeof (more);
    while(more) {
        zmq::message_t response;
        socket.recv(&response);

        std::string response_str((const char *)response.data(), response.size());
        cout << response_str << endl;

        socket.getsockopt(ZMQ_RCVMORE, &more, &more_size);
    }
}

int main(int argc, char **argv) {
    if (argc < 3) {
        usage();
    }

    int socket_type = get_socket_type(argv[1]);

    zmq::context_t context(1);
    zmq::socket_t socket(context, socket_type);
    socket.connect(argv[2]);

    if (argc == 3) {
        zmq::message_t empty_message;
        socket.send(empty_message);

        recv_multipart(socket);
    }
    else {
        for (int i=3; i<argc; ++i) {
            size_t arg_len= std::strlen(argv[i]);
            zmq::message_t message(arg_len);
            std::memcpy(message.data(), argv[i], arg_len);
            socket.send(message, ZMQ_SNDMORE && ( (i+1) < argc) );
        }

        recv_multipart(socket);
    }

    return 0;
}




















/* vim: set expandtab ts=4 sw=4 sts=4 tw=100: */
