require "zmq"
local context = zmq.init( 1 )

print( "connect to hello world server..." )
local socket = context:socket( zmq.REQ )
socket:connect( "tcp://localhost:5555" )
for n=1,10 do
	print( "Sending Hello ".. n .. " ..." )
	socket:send( "Hello" )
	local reply = socket:recv( )
	print( "Received World ".. n .. " ["..reply.."]" )
end

socket:close( )
context:term( )
