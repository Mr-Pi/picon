-define(NYI_T, lager:debug("not yet implemented"), throw(not_yet_implemented)).
-define(NYI, lager:debug("not yet implemented")).
-define(APPLICATION, picon).

-record(connection, {state, node, reference, remaining}).

-type nodes() :: [node()].
