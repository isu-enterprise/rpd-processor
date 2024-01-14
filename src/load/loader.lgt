:- set_prolog_flag(stack_limit, 8_147_483_648).

:- initialization((
     set_logtalk_flag(report, warnings)
   , set_logtalk_flag(events, allow)
   , set_logtalk_flag(debug, on)
   , set_prolog_flag(verbose_load, true)
   , logtalk_load(tutor(loader))
   , logtalk_load(tools(loader))
   , logtalk_load(debugger(loader))

   , logtalk_load([
          config
        , xlsx
        , app
        ])
   , halt
)).
