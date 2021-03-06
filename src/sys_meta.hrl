%%% File : system.hrl
%%% Description : Include file for system db
%%% system stores metadata of the cluster


-record(sys_meta,
	{id       = 1,			%int()
	 name     = my_awesome_cluster, % atom()
	 status   = closed,		% closed | restricted | open
	 last_scn = 200,		%int()
	 nodes    = [
		  ra1@localhost,
		  ra2@localhost,
		  ra3@localhost,
		  ra4@localhost
		 ],                    	%[string()], list of nodes
	 ts       = []                  %[{atom(),[string()]}], list of tuple spaces names 
	                                %with associated nodes

	}).                             %[string()], list of nodes

