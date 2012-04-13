#include <zmq.h>
#include <stdio.h>
#include <unistd.h>
#include <iostream>
#include <string>

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
		people.ParseFromString( pack );
		zmq_msg_close( &request );

		sleep(1);

		zmq_msg_t reply;
		zmq_msg_init_size( &reply, 5 );
		memcpy( zmq_msg_data( &reply ), "World", 5 );
		zmq_send( responder, &reply, 0 );
		zmq_msg_close( &reply );
	}

	zmq_close( responder );
	zmq_term( context );
	return 0;
}
