:- category(text_syllabus_sections,
    extends(text_sections)).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-09-23,
        comment is 'Specifies parameters for syllabus sections recognition'
    ]).

    :- public(process_syllabus_sections/0).
    :- info(process_syllabus_sections/0, [
        comment is 'Run recognition of syllabus sections'
    ]).

    process_syllabus_sections :-
        ::process_sections.

    :- protected(number_section/4).
    :- info(number_section/0, [
        comment is 'Description'
    ]).

    number_section(1, aims_and_problems, none).
    number_section(2, aims_and_problems, none).
    number_section(3, requirements).
    number_section(4, content, none).
    number_section([4,1], content_hours, content).
    number_section([4,2], individuals_holur, content).
    number_section([4,3], content_structure, content).
    number_section([4,3,1], practics, content_structure).
    number_section([4,3,2], individuals, content_structure).
    number_section([4,4], individual_technique, content).
    number_section(5, info_provision, none).
    number_section(6, tech_provision, none).
    number_section([6,1], equipment, tech_provision).
    number_section([6,2], software, tech_provision).
    number_section(7, control_matter, none).
    number_section([7,1], current_control, control).
    number_section([7,2], intermediate_control, control).

    :- protected(section_title/2).
    :- info(section_tite/2, [
        comment is 'Set of ISU standard titles'
    ]).

    section_title(aims_and_problems, 'Цели и задачи').
    section_title(placement, 'Место дисциплины в структуре ОПОП ВО').
    section_title(requirements, 'Требования к результатам освоения дисциплины').
    section_title(content, 'Содержание и структура дисциплины').
    section_title(content_hours, 'Содержание дисциплины, структурированное по темам, c указанием видов учебных занятий и отведенного на них количества академических часов').
    section_title(individuals_holur, 'План внеаудиторной самостоятельной работы обучающихся по дисциплине').
    section_title(content_structure, 'Содержание учебного материала').
    section_title(practics, 'Перечень семинарских, практических занятий и лабораторных работ').
    section_title(individuals, 'Перечень тем (вопросов)., выносимых на самостоятельное изучение студентами в рамках самостоятельной работы').
    section_title(individual_technique, 'Методические указания по организации самостоятельной работы студентов').
    section_title(info_provision, 'Учебно-методическое и информационное обеспечение дисциплины').
    section_title(tech_providion, 'Материально-техническое обеспечение дисциплины').
    section_title(equipment, 'Учебно-лабораторное оборудование').
    section_title(software, 'Программное обеспечение').
    section_title(control, 'Оценочные материалы для текущего контроля и промежуточной аттестации').
    section_title(current_control, 'Оценочные средства текущего контроля').
    section_title(intermediate_control, 'Оценочные средства для промежуточной аттестации').


:- end_category.
