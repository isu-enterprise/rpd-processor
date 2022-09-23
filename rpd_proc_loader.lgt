:- set_prolog_flag(stack_limit, 8_147_483_648).

:- initialization((
    % set project-specific global flags
    set_logtalk_flag(report, warnings),
    set_logtalk_flag(events, allow),
    set_logtalk_flag(debug, on),
    set_prolog_flag(verbose_load, true),
    logtalk_load(tutor(loader)),
    logtalk_load(tools(loader)),  % debugging, tracing, trace
    logtalk_load(debugger(loader)),  % debugging

    % load the project source files
    logtalk_load([
        'src/xml_loader_1',
        'src/as_db_1',
        'src/text_attrib',
        'src/text_features',
        'src/text_merge',
        'src/text_sections',
        'src/text_syllabus_sections',
        'src/recognizer_1',

        'src/test']),
    % debugger::debug,
    % pkt_run::run,
    test::run,
    test::attrib_test,
    true
)).
