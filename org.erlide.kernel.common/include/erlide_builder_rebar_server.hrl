
-spec init(Args :: term()) -> Result when
          Result :: {ok, StateName, StateData}
              | {ok, StateName, StateData, Timeout}
              | {ok, StateName, StateData, hibernate}
              | {stop, Reason}
              | ignore,
StateName :: atom(),
StateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: term().

-spec init(Event :: timeout | term(), StateData :: term()) -> Result when
          Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, hibernate}
              | {stop, Reason, NewStateData},
NextStateName :: atom(),
NewStateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: term().

-spec compile(Event :: timeout | term(), StateData :: term()) -> Result when
          Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, hibernate}
              | {stop, Reason, NewStateData},
NextStateName :: atom(),
NewStateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: term().

-spec compile_1(Event :: timeout | term(), StateData :: term()) -> Result when
          Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, hibernate}
              | {stop, Reason, NewStateData},
NextStateName :: atom(),
NewStateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: term().

-spec eunit(Event :: timeout | term(), StateData :: term()) -> Result when
          Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, hibernate}
              | {stop, Reason, NewStateData},
NextStateName :: atom(),
NewStateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: term().

-spec clean(Event :: timeout | term(), StateData :: term()) -> Result when
          Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, hibernate}
              | {stop, Reason, NewStateData},
NextStateName :: atom(),
NewStateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: term().

-spec ignoring(Event :: timeout | term(), StateData :: term()) -> Result when
          Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, hibernate}
              | {stop, Reason, NewStateData},
NextStateName :: atom(),
NewStateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: term().

-spec xref(Event :: timeout | term(), StateData :: term()) -> Result when
          Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, hibernate}
              | {stop, Reason, NewStateData},
NextStateName :: atom(),
NewStateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: term().

-spec handle_event(Event :: term(), StateName :: atom(), StateData :: term()) -> Result when
          Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, hibernate}
              | {stop, Reason, NewStateData},
NextStateName :: atom(),
NewStateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: term().

-spec handle_info(Info :: term(), StateName :: atom(), StateData :: term()) -> Result when
          Result :: {next_state, NextStateName, NewStateData}
              | {next_state, NextStateName, NewStateData, Timeout}
              | {next_state, NextStateName, NewStateData, hibernate}
              | {stop, Reason, NewStateData},
NextStateName :: atom(),
NewStateData :: term(),
Timeout :: non_neg_integer() | infinity,
Reason :: normal | term().

-spec terminate(Reason, StateName :: atom(), StateData :: term()) -> Result :: term() when
          Reason :: normal
              | shutdown
              | {shutdown, term()}
              | term().

-spec code_change(OldVsn, StateName :: atom(), StateData :: term(), Extra :: term()) -> {ok, NextStateName :: atom(), NewStateData :: term()} when
          OldVsn :: Vsn | {down, Vsn},
Vsn :: term().

