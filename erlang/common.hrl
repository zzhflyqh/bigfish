%%tcp_server listen params

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}, {nodelay, false},
		{delay_send, true},{send_timeout, 5000}, {keepalive, true}, {exit_on_close, true}]).

-define(HOST, "127.0.0.1").
-define(PORT, 8888).
