-module(simple_queue).
-export([new/0, in/2, out/1]).

new() -> {[],[]}.

in( X, {In, Out} ) -> {[X|In], Out}.

out( Empty= {[], []} ) -> {undefined, Empty};
out( {[X], []} ) -> {X, {[], []}};
out( {In, []} ) -> [X|Out]= lists:reverse(In), {X, {[], Out}};
out( {In, [X|Out]} ) -> {X, {In, Out}}.
