-module(skyjet).

-export([start/0]).
-export([stop/0]).
-export([restart/0]).

start() -> application:start(?MODULE).

stop() -> application:stop(?MODULE).

restart() -> stop(), start().
