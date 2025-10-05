# Crontab

Crontab for Erlang, a simple scheduling application.

This is suitable for long-running applications that needs to run periodic jobs. It's similar to
the cron utility but in Erlang.

## Example usage

Start with compiling and opening a shell ``rebar3 compile && rebar3 shell``:

```erlang
ok = application:start(crontab).

MFA = {io, format, ["hello from crontab\n"]}.

MFA2 = {lists, map, [bad_args]}.

%% every minute
ok = crontab:add(job1, ['*', '*', '*',  '*', '*'], MFA1).

ok = crontab:remove(job1).

%% once/week, monday 8.30
ok = crontab:add(job2, ['*', '*', monday,  8, 30], MFA1).

%% first day of month, 00.00
ok = crontab:add(job3, ['*', '*', 1, 0, 0], MFA1).

%% 29th of february, 20.00
ok = crontab:add(job4, ['*', 2, 29, 20, 0], MFA1).

%% every 15 minute on mondays and fridays
ok = crontab:add(job5, ['*', '*', [monday, friday], '*', [0, 15, 30, 45]], MFA1).

%% failing job every minute
ok = crontab:add(job6, ['*', '*', '*',  '*', '*'], MFA2).
```

Each execution of a job will have it's own process and success/failure is logged.

## API

API consists of `crontab:add/3`, `crontab:remove/1` and `crontab:remove/2`.

```erlang
crontab:add(Name::atom(), Spec::list(), MFA::mfa()) -> ok | {error, task_exists} | {error, no_next_found}.
crontab:remove(Name::atom()) -> ok | {error, no_such_task}.
crontab:remove(atom(), list()) -> ok | {error, no_such_task}.
```

```erlang
Spec = [year(), month(), day(), hour(), min()]

year()  :: 0..N  | '*' | [2013, 2015, ..]
month() :: 1..12 | '*' | [1, 2, 3, ..]
day()   :: 1..31 | monday | tuesday | wednesday | thursday | friday |
           saturday | sunday | [monday, 21, friday, ..]
hour()  :: 0..23 | '*' | [12, 22, ..]
min()   :: 0..59 | '*' | [0, 15, 30, 45, ..]
```

## TODO

* Make testcases run faster
* Ensure all parts are tested
