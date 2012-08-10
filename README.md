<h1>scheduler</h1>

=====================

__Authors:__ Bjorn Jensen-Urstad ([`bjorn.jensen.urstad@gmail.com`](mailto:bjorn.jensen.urstad@gmail.com)).

Execute scheduled functions.

API is consists of scheduler:add/3 and scheduler:del/1.

<pre>
  scheduler:add(Name::atom(), Spec::list(), del::atom()) -> ok | {error, Rsn}
  scheduler:del(Name::atom()) -> ok | {error, Rsn}

  Spec = [Year, Month, Day, Hour, Min]

  Year  = 0..N  | "*"
  Month = 1..12 | "*"
  Day   = 1..31 | monday | tuesday | wednesday | thursday | friday |
          saturday | sunday
  Hour  = 0..23 | "*"
  Min   = 0..59 | "*"
</pre>


Example usage:
<pre>
  ok = application:start(scheduler),
  MFA = {module, function, args},
  ok = scheduler:add(first_dow, {"*", "*", monday,  8, 30}, MFA),
  ok = scheduler:add(first_dom, {"*", "*", 1,       0,  0}, MFA),
  ok = scheduler:add(feb29th,   {"*", 2,   29,      20, 0}, MFA),

  ok = scheduler:del(first_dow),
  ok = application:stop(scheduler),
</pre>


* Manifest
* eof
