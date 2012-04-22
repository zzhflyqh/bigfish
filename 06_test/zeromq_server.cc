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
	void *context = zmq_init(1);
	void *responder = zmq_socket( context, ZMQ_REQ );
	zmq_bind( responder, "tcp://*:5555" );
	while(1){
		demo::People people;
		zmq_msg_t request;
		zmq_msg_init( &request );
		zmq_recv( responder, &request, 0 );
		std::string pack = (char*) zmq_msg_data( &request );
        if(pack.length() > 0){
            people.ParseFromString( pack );
            fprintf( stderr, "id: %d, name: %s, email: %s", people.id(), people.name().c_str(), people.email( ).c_str() );
            zmq_msg_close( &request );

            sleep(1);

            zmq_msg_t reply;
            std::string reply_pack = std::string( "Hello," ) + people.name();
            zmq_msg_init_size( &reply, reply_pack.length() );
            memcpy( zmq_msg_data( &reply ), reply_pack.c_str(), reply_pack.length() );
            zmq_send( responder, &reply, 0 );
            zmq_msg_close( &reply );
        }
	}

	zmq_close( responder );
	zmq_term( context );
	return 0;
}
