picon
=====

erlang application, to easily connect and monitoring erlang nodes.

**!!! NOTHING IMPLEMENTED YET !!!**

Records
-------

All records a defined in *include/picon.hrl*

	#connection{
		state :: connected | disconnected | waiting | reconnecting | removed | timeout | undefined,
		type :: picon:connection_type(),
		retrials :: {non_neg_integer(), non_neg_integer()}
		}.

Types
-----

| Type                    | Definition                 | Description                                    |
| ----------------------- | -------------------------- | ---------------------------------------------- |
| picon:node_list()       | atom()                     | reference to a list from config                |
| picon:nodes()           | [node()]                   | A list of erlang nodes                         |
| picon:connection_type() | temporary &#124; permanent | temporary nodes, don't reconnect automatically |
| picon:connection()      | #connection{}              | connection is defined in *include/picon.hrl*   |


Configuration
-------------

To configure picon, simply set the application environment:

You can set followring options:

| Parameter           | Value Type                    | Default | Description                                                       |
| ------------------- | ----------------------------- | ------- | ----------------------------------------------------------------- |
| lists               | [{atom(),[node()]}]           | []      | specified a lists of erlang nodes, atom() is the name of a list   | 
| auto_connect        | [{atom(), connection_type()}] | []      | connects a list of nodes a startup                                |
| auto_connect_remote | boolean()                     | false   | calls `picon:connect(local)` on added nodes                       |
| max_retrials        | non_neg_integer()             | 720     | specified the number of retrials                                  |
| retrials_interval   | non_neg_integer()             | 5000    | specified the break between the retrials                          |


Callback functions
------------------

| Function                                                                                      | Return specification                                    | Note                                |
| --------------------------------------------------------------------------------------------- | ------------------------------------------------------- | ----------------------------------- |
| picon:connect(node() &#124; nodes() &#124; picon:node_list() &#124; local, connection_type()) | [{node(), picon:connection{}] &#124; picon:connection() | node() must be a fullqualified name |
| picon:remove(node() &#124; nodes() &#124; picon:node_list() &#124; local)                     | ok                                                      | **- '' -**                          |
| picon:modify(node() &#124; nodes() &#124; picon:node_list(), connection_type())               | ok                                                      | **- '' -**                          |
| picon:getStatus(node() &#124; nodes() &#124; picon:node_list() &#124; local &#124; any)       | [{node(), picon:connection{}] &#124; picon:connection() | **- '' -**                          |

