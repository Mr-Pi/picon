-define(NYI_T, lager:debug("not yet implemented"), throw(not_yet_implemented)).
-define(NYI, lager:debug("not yet implemented")).
-define(APPLICATION, picon).

-record(connection, {
		state :: connected | disconnected | waiting | reconnecting | removed | timeout | undefined,
		node :: node() ,
		type :: permanent | temporary,
		retrials={0,0} :: {non_neg_integer(), non_neg_integer()}
		}).

-type nodes() :: [node()].
