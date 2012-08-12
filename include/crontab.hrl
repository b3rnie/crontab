-ifndef(__SCHEDULER_HRL__).
-define(__SCHEDULER_HRL__, false).

-ifndef(hence).
-define(hence(X),([throw({assert, {??X,'=',true}, ?FILE, ?LINE}) || X /= true])).
-endif.

-ifndef(debug).
-define(debug(F),   ?debug(F,[])).
-define(debug(F,A), error_logger:info_msg(F,A)).
-endif.

-ifndef(warning).
-define(warning(F),   ?warning(F, [])).
-define(warning(F,A), error_logger:warning_msg(F,A)).
-endif.

-endif.
