-define(NYI_T, lager:debug("not yet implemented"), throw(not_yet_implemented)).
-define(NYI, lager:debug("not yet implemented")).
-define(APPLICATION, picon).

-record(connection, {state :: connected | disconnected | waiting | reconnecting | removed, node :: node() , retrials :: non_neg_integer() | none}).
-define(CONNECTED(Node), #connection{state=connected, node=Node}).
-define(DISCONNECTED(Node), #connection{state=disconnected, node=Node}).

-type nodes() :: [node()].
