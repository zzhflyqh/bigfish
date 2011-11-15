{application,gameserver,
             [{description,"gameserver demo"},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{gameserver_app,[]}},
              {env,[]},
              {modules,[gameserver_app,gameserver_sup,gs_server_net,
                        mod_rand]}]}.
