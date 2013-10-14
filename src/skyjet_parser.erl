-module(skyjet_parser).

-export([start_link/1]).
-export([init/2]).
-export([handle_info/2]).

-record(state, {socket, handler, msg, tags = []}).

-record(startElement, {uri, lname, qname, attributes = []}).
-record(endElement, {uri, lname, qname}).

-include("messages.hrl").

start_link(Socket) ->
	{ok, Pid} = proc_lib:start_link(?MODULE, init, [Socket, self()]),
	gen_tcp:controlling_process(Socket, Pid),
	{ok, Pid}.

init(Socket, Handler) ->
	proc_lib:init_ack({ok, self()}),
	State = #state{socket = Socket, handler = Handler},
	xmerl_sax_parser:stream(<<>>, [
																 {encoding, utf8},
																 {event_fun, fun event/3},
																 {event_state, State},
																 {continuation_fun, fun loop/1},
																 {continuation_state, State}
	]).

loop(#state{socket = Socket} = State) ->
	lager:debug("receive loop"),
	Result = receive
		{tcp, Socket, Msg} ->
			lager:info("tcp msg ~p", [Msg]),
			{reply, {Msg, State}};
		{tcp_closed, Socket} ->
			throw(connection_closed);
		Msg ->
			erlang:apply(?MODULE, handle_info, [Msg, State])
	end,
	case Result of
		{reply, Value} -> Value;
		_ -> loop(State)
	end.

handle_info({send, Handler, Msg}, #state{socket = Socket, handler = Handler}) ->
	send(Socket, Msg),
	ok;
handle_info(Msg, _) ->
	lager:warning("msg ~p", [Msg]),
	ok.

send(Socket, Msg) ->
	lager:debug("sending data ~p", [Msg]),
	gen_tcp:send(Socket, Msg).

handle(Msg, State) ->
	lager:info("sending msg ~p", [Msg]),
	State#state.handler ! {xmpp, self(), Msg},
	State.

event(#startElement{
				 uri = "http://etherx.jabber.org/streams",
				 lname = "stream",
				 qname={"stream", "stream"},
				 attributes = Attributes},
			_, State) ->
	[{_Uri, _Prefix, "id", Id}] = [X || X <- Attributes, element(3, X) =:= "id"],
	handle({stream_id, Id}, State),
	State;

event(Element, _Location, #state{msg = Msg} = State)
	when
		element(1, Element) =:= startElement
		->
	NewState = pre_event(Element, State),
	NewMsg = event(Element, Msg),
	NewState#state{msg = NewMsg};

event(Element, _Location, State)
	when
		element(1, Element) =:= endElement
		->
	post_event(Element, State);

event(El, _, State)
	when
		element(1, El) =:= startPrefixMapping;
		element(1, El) =:= ignorableWhitespace;
		element(1, El) =:= endPrefixMapping;
		El =:= startDocument
		-> State;

event(Elem, _Loc, State) ->
	lager:warning("element ~p", [Elem]),
	lager:warning("state ~p", [State]),
	State.

pre_event(#startElement{qname = QName}, #state{tags = Tags} = State) ->
	State#state{tags = [QName | Tags]}.

post_event(#endElement{qname = QName},
					 #state{
							tags = [QName],
							msg = Msg
						 } = State)
	when
		Msg =/= undefined
		->
	handle(Msg, State),
	State#state{msg = undefined, tags = []};

post_event(#endElement{qname = QName}, #state{tags = [QName | Tags]} = State) ->
	State#state{tags = Tags}.

event(#startElement{lname = "iq", attributes = Attributes}, undefined) ->
	Attr = [{X, Y} || {_, _, X, Y} <- Attributes],
	From = proplists:get_value("from", Attr),
	To = proplists:get_value("to", Attr),
	Type = proplists:get_value("type", Attr),
	Id = proplists:get_value("id", Attr),
	#iq{from = From, to = To, type = Type, id = Id};

event(#startElement{
				 uri = Uri,
				 lname = LName},
			#iq{} = Msg) ->
	Msg#iq{q = {LName, Uri}};

event(#startElement{lname = "remove"}, #iq{} = Msg) ->
	Msg#iq{remove = true};

event(#startElement{lname = "presence", attributes = Attributes}, undefined) ->
	Attr = [{X, Y} || {_, _, X, Y} <- Attributes],
	From = proplists:get_value("from", Attr),
	To = proplists:get_value("to", Attr),
	Type = proplists:get_value("type", Attr),
	#presence{from = From, to = To, type = Type};

event(#startElement{} = El, Msg) ->
	Msg;

	event({characters, Value}, Msg) ->
	set(Msg, Value).

set(Msg, _Value) -> Msg.
