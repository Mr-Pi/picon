-define(NYI, not_yet_implemented, lager:warning("NOT YET IMPLEMENTED")).

-record(connection, {
	  state :: connected | disconnected | waiting | reconnecting | removed | timeout | undefined,
	  type :: picon:connection_type(),
	  reconnects :: non_neg_integer(),
	  retrials :: non_neg_integer()
	 }).
