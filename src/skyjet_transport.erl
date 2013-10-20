-module(skyjet_transport).

-behaviour(jet).

-export([init/2]).
-export([start_link/4]).
-export([disco_info/2]).
-export([register_info/2]).

-include_lib("jet/include/messages.hrl").

-record(state, {name}).

start_link(Name, Host, Port, Passwd) ->
	jet:start_link({local, ?MODULE}, ?MODULE, [Name, Host, Port, Passwd], []).

init(Name, Opts) ->
	{ok, #state{name = Name}}.

disco_info(Msg, State) ->
	lager:debug("disco info"),
	Reply = [ 
			"<identity category='gateway' ",
			"type='skype' ",
			"name='Skype Gateway'/>",
			"<feature var='http://jabber.org/protocol/disco#info'/>",
			"<feature var='jabber:iq:register'/>",
			"<feature var='vcard-temp'/>"
	],
	{reply, Reply, State}.

register_info(Msg, State) ->
	lager:debug("register info"),
	Reply = [
			"<instructions>",
			"Press any key.",
			"</instructions>"],
	{reply, Reply, State}.
