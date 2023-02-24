https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
-module(midterm_tests).

-include_lib("eunit/include/eunit.hrl").

-export([reduce_bu_test/1, scan_bu_test/1]).
-export([cumsum/1, cumsum/2, max_plus_mapfoldl/2]).

% reduce_bu_test(EUnit)
%   If EUnit == true,
%     then generate a list of EUnit tests.
%   Otherwise, run the tests that EUnit would run, but print a more human
%   friendly description of the outcome (EUnit suppresses output from test
%   execution).
reduce_bu_test(EUnit) when is_boolean(EUnit) ->
  do_it(reduce_bu, EUnit).
% reduce_bu_test_() -> reduce_bu_test(true).


% scan_bu_test(EUnit)
%   Like reduce_bu_test.
%   If EUnit == true, generate EUnit tests;
%   otherwise, run the tests and print the results in a human friendly way.
scan_bu_test(EUnit) when is_boolean(EUnit) -> do_it(scan_bu, EUnit).
scan_bu_test_() -> scan_bu_test(true).


do_it(ReduceOrScan, true) ->
  [    do_it(ReduceOrScan, TestCase, true)
    || TestCase <- test_cases()];
do_it(ReduceOrScan, false) ->
  do_it(ReduceOrScan, test_cases());
do_it(_, []) -> [];
do_it(ReduceOrScan, [TestCase | TestCasesTail]) ->
  case do_it(ReduceOrScan, TestCase, false) of
    {fail, timeout} -> [timeout];
    X -> [X | do_it(ReduceOrScan, TestCasesTail)]
  end.

do_it(ReduceOrScan, {P, N, DataFun, LeafFun, CombineFun, Leaf2Fun, AccIn}, EUnit) ->
  Test = { setup,
    fun() ->  red:create(P) end, % set-up
    fun(W) -> red:reap(W) end, % clean up
    fun(W) -> % run
      Data = DataFun(N, P),
      red:update(W, data, Data),
      red:broadcast(W,
	fun(ProcState) ->
	  MyData = red:get(ProcState, data),
	  Total = case ReduceOrScan of
		    reduce_bu -> midterm:reduce_bu(ProcState, LeafFun(MyData), CombineFun);
		    scan_bu ->
		      AccLeft = midterm:scan_bu(ProcState, AccIn, LeafFun(MyData), CombineFun),
		      Leaf2Fun(AccLeft, MyData)
		  end,
	  red:put(ProcState, total, Total)
	end),
      {Expected, Actual} =
        case ReduceOrScan of
	  reduce_bu -> {LeafFun(Data), hd(red:retrieve(W, total))};
	  scan_bu   -> {Leaf2Fun(AccIn, Data), lists:append(red:retrieve(W, total))}
	end,
      case {EUnit, Expected =:= Actual} of
        {false, true} ->
	  io:format("~w(P=~w): passed~n  Data = ~w~n  Sum = ~w~n",
		    [ReduceOrScan, P, Data, Actual]);
        {false, false} ->
	  io:format("~w(P=~w): FAILED~n  Data = ~w~n  Expected result =~w~n  Result from ~w = ~w~n",
		    [ReduceOrScan, P, Data, Expected, ReduceOrScan, Actual]),
	  fail;
	{true, _} -> ?_assertEqual(Expected, Actual)
      end
    end
  },
  case EUnit of
    true -> Test;
    false -> % run the test now
      MyPid = self(),
      {setup, SetUp, CleanUp, Run} = Test,
      W = SetUp(),
      TestPid = spawn(fun() ->
	Result = Run(W),
	MyPid ! {test_result, self(), Result}
      end),
      io:format("do_it: waiting for Godot~n"),
      TestResult = receive
	{test_result, TestPid, ReceivedResult} -> ReceivedResult
      after 5000 -> {fail, timeout}
      end,
      CleanUp(W),
      TestResult
  end.

cumsum(AccIn, List) ->
  element(1, lists:mapfoldl(fun(X, Acc) -> Y = Acc+X, {Y, Y} end, AccIn, List)).
cumsum(List) -> cumsum(0, List).

test_cases() ->
  [    list_to_tuple([P, test_n(P) | Funs])
    || P <- [4, 8, 1, 14, 105, 256],
       Funs <- [ [ fun(N, _) -> misc:rlist(N, 1000) end, % DataFun
		   fun lists:sum/1, % LeafFun
		   fun(X, Y) -> X+Y end, % CombineFun
		   fun(AccIn, MyData) -> cumsum(AccIn, MyData) end, % Leaf2Fun (for scan)
		   0  % AccIn
		 ], % CombineFun
		 [ fun(N, _) -> lists:zip([ X-6 || X <- misc:rlist(N, 11)],
					  [ round(math:sqrt((R+5)*I))
					    || {R, I} <- lists:zip(misc:rlist(N, 11),
								   lists:seq(1,N))])
		   end,
		   fun(List) ->
		     element(2, max_plus_mapfoldl(List, {0, neg_infinity}))
		   end,
		   fun({LeftPlus, LeftMax}, {RightPlus, RightMax}) ->
		       {LeftPlus + RightPlus,
			case {LeftMax, RightMax} of
			  {neg_infinity, _} -> RightMax;
			  {_, neg_infinity} -> LeftMax;
			  {_, _} -> max(LeftMax + RightPlus, RightMax)
			end
		       }
		   end,
		   fun(AccLeft, List) -> element(1, max_plus_mapfoldl(List, AccLeft)) end,
		   {0, neg_infinity}]]].
test_n(14) -> 9;
test_n(P) -> P*round(math:sqrt(P) + 5).

max_plus_mapfoldl(List, AccIn) ->
  lists:mapfoldl(
    fun({Plus, Max}, {AccPlus, AccMax}) ->
      V = { AccPlus + Plus,
	    case AccMax of
	      neg_infinity -> Max;
	      _ ->max(AccMax+Plus, Max)
	    end},
      {V, V}
    end,
    AccIn,
    List).
