#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <iostream>
#include <string>

#include "zeromq_help.h"
#include "demo.people.pb.h"

using namespace std;

int main( void )
{
    void *context = zmq_init( 1 );
    void *requester = zmq_socket( context, ZMQ_REQ );
    zmq_connect( requester, "tcp://*:5555" );
    demo::People people;
    people.set_name( "zhangzhihua" );
    people.set_id( 123 );
    people.set_email( "1@2.com" );
    std::string req_pack = "";
    people.SerializeToString( &req_pack );
    s_send( requester, req_pack.c_str( ) );
    char *string = s_recv (requester);
    printf ("Received reply [%s]\n", string);
    free (string);
    zmq_close( requester );
    zmq_term( context );
}
