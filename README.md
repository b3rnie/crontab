<h1>crontab</h1>

=====================

__Authors:__ Bjorn Jensen-Urstad ([`bjorn.jensen.urstad@gmail.com`](mailto:bjorn.jensen.urstad@gmail.com)).


Execute scheduled functions.

API is consists of scheduler:add/3 and scheduler:del/1.

<pre>
  scheduler:add(Name::atom(), Spec::list(), MFA) -> ok | {error, Rsn}
  scheduler:del(Name::atom()) -> ok | {error, Rsn}

  Spec = [Year, Month, Day, Hour, Min]

  Year  = 0..N  | "*"
  Month = 1..12 | "*"
  Day   = 1..31 | monday | tuesday | wednesday | thursday | friday |
          saturday | sunday
  Hour  = 0..23 | "*"
  Min   = 0..59 | "*"

  MFA = {module, function, args}
</pre>


Example usage:
<pre>
  ok = application:start(crontab),
  MFA = {module, function, []},

  %% once/week, monday 8.30
  ok = scheduler:add(foo, {"*", "*", monday,  8, 30}, MFA),
  %% first day of month, 00.00
  ok = scheduler:add(bar, {"*", "*", 1, 0, 0}, MFA),
  %% 29th of february, 20.00
  ok = scheduler:add(baz, {"*", 2, 29, 20, 0}, MFA),

  ok = scheduler:del(foo),
  ok = application:stop(crontab),
</pre>


* Manifest
* eof
