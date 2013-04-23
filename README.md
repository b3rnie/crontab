# Crontab

Crontab for Erlang.

Execute scheduled functions.

## Authors

* [Björn Jensen-Urstad](mailto:bjorn.jensen.urstad@gmail.com)

## API

API consists of `crontab:add/3` and `crontab:remove/1`.

```erlang
crontab:add(Name::atom(), Spec::list(), MFA::mfa()) -> ok | {error, Rsn}
crontab:remove(Name::atom()) -> ok | {error, Rsn}
```

```erlang
Spec = [year(), month(), day(), hour(), min()]

year()  :: 0..N  | '*' | [2013, 2015, ..]
month() :: 1..12 | '*' | [1, 2, 3, ..]
day()   :: 1..31 | monday | tuesday | wednesday | thursday | friday |
           saturday | sunday | [monday, 21, friday, ..]
hour()  :: 0..23 | '*' | [12, 22, ..]
min()   :: 0..59 | '*' | [0, 15, 30, 45, ..]

MFA = {module, function, args}
```

## Usage

Example:

```erlang
ok = application:start(crontab),
MFA = {module, function, []},

%% once/week, monday 8.30
ok = crontab:add(foo, ['*', '*', monday,  8, 30], MFA),
%% first day of month, 00.00
ok = crontab:add(bar, ['*', '*', 1, 0, 0], MFA),
%% 29th of february, 20.00
ok = crontab:add(baz, ['*', 2, 29, 20, 0], MFA),
%% every 15 minute on mondays and fridays
ok = crontab:add(buz, ['*', '*', [monday, friday], '*', [0, 15, 30, 45]], MFA),
ok = application:stop(crontab),
```

## Manifest
