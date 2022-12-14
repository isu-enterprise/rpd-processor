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
        'src/htmlize',
        'src/as_db_1',
        'src/fonts',
        'src/degraded',
        'src/syllabus_fonts',
        'src/text_attrib',
        'src/text_features',
        'src/text_merge',
        'src/syllabus_merge',
        'src/syllabus_page_one',
        'src/text_sections',
        'src/text_syllabus_sections',   % configuration
        'src/gather_items',
        'src/text_fields',
        'src/text_syllabus_fields',     % configuration
        'src/grouping',

        'src/syllabus_recognizer_1',

        'src/test']),
    test::run,
    test::attrib_test,
    true
)).
