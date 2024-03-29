:- category(text_syllabus_sections,
    extends(text_sections)).

    :- info([
        version is 1:0:0,
        author is 'Evgeny Cherkashin <eugeneai@irnok.net>',
        date is 2022-12-08,
        comment is 'Specifies parameters for LETI course description sections recognition'
    ]).

    :- public(process_syllabus_sections/0).
    :- info(process_syllabus_sections/0, [
        comment is 'Run recognition of course description sections'
    ]).

    process_syllabus_sections :-
        ::process_sections.

    :- protected(number_section/4).
    :- info(number_section/4, [
        comment is 'Defines a numbered section'
    ]).

    number_section(1, discipline_structure, none, [структур, дисциплин]).
    number_section(2, discipline_annotation, none, [аннотац, дисциплин]).
    number_section(3, generics, none, [общ, положен]).
    number_section([3,1], aims_and_problems, generics, [цел, задач]).
    number_section([3,2], placement, generics, [мест, дисциплин, структур]).
    number_section([3,3], ensurement, generics, [перечен, планир, результат, обучен, дисциплин]).
    number_section(4, content, none, [содержан, дисциплин]).

    number_section([4,1], content_hours, content, [наименован, тем, час, вид, нагрузк]).


    number_section([4,2], individuals_holur, content, [план, самостоятельн, работ]).
    number_section([4,3], content_structure, content, [содержан, учебн, материал]).
    number_section([4,3,1], practics, content_structure, [перечен, занят, работ]).
    number_section([4,3,2], individuals, content_structure, [перечен, тем, самостоятельн, работ]).
    number_section([4,4], individual_technique, content, [методическ, указан, самостоятельн, работ]).
    number_section(5, info_provision, none, [учебн, методическ, информацион, обеспечен, дисциплин]).
    number_section(_, main_references, info_provision, [')', основн, литератур]).
    number_section(_, auxiliary_references, info_provision, [')', дополнительн, литератур]).
    number_section(_, electronic_references, info_provision, [')', баз, данн, информационн, справочн, систем]).
    number_section(_, document_references, info_provision, [')', нормативн, документ]).
    number_section(6, tech_provision, none, [материал, техническ, обеспечен, дисциплин]).
    number_section([6,1], equipment, tech_provision, [учебн, лабораторн, оборудован]).
    number_section([6,2], software, tech_provision, [программн, обеспечен]).
    number_section(7, control_matter, none, [оценочн, текущ, контрол, аттестац]).
    number_section([7,1], current_control, control_matter, [оценочн, текущ, контрол]).
    number_section([7,2], intermediate_control, control_matter, [оценочн, промежуточн, аттестац]).

    :- protected(unnumbered_section/3).
    :- info(unnumbered_section/3, [
        comment is 'Section definition without numbers'
    ]).

    unnumbered_section(discipline_annotation_en, discipline_annotation, [subject, summary]).




    unnumbered_section(ensured_courses, placement, [перечен, последующ, дисципл, формируем, дисципл]).
    unnumbered_section(current_control_examples, current_control, [пример, оценочн, средств, текущ, контрол]).
    unnumbered_section(intermediate_control_questions, intermediate_control, [список, вопрос, промежуточн, аттестац]).
    unnumbered_section(intermediate_control_examples, intermediate_control, [пример, оценочн, средств, промежуточн, аттестац]).
    unnumbered_section(author, nosection, [разработч, ":"]).

    :- protected(section_title/2).
    :- info(section_title/2, [
        comment is 'Set of LETI standard titles'
    ]).

    section_title(discipline_structure, 'Структура дисциплины').
    section_title(discipline_annotation, 'Аннотация дисциплины').
    section_title(generics, 'Общие положения').
    section_title(aims_and_problems, 'Цели и задачи').
    section_title(placement, 'Место дисциплины в структуре ОПОП').
    section_title(ensurement, 'Перечень планируемых результатов обучения по дисциплине, соотнесенных с планируемыми результатами освоения образовательной программы').
    section_title(content, 'Содержание дисциплины').




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
    section_title(control_matter, 'Оценочные материалы для текущего контроля и промежуточной аттестации').
    section_title(current_control, 'Оценочные средства текущего контроля').
    section_title(intermediate_control, 'Оценочные средства для промежуточной аттестации').

    % Unnumbered sections
    section_title(discipline_annotation_en, 'Subject Summary').




    section_title(ensured_courses, 'Перечень последующих учебных дисциплин, для которых необходимы знания, умения и навыки, формируемые данной учебной дисциплиной').

    section_title(main_references, [а, "основная литература:"]).
    section_title(auxiliary_references, [б, "дополнительная литература:"]).
    section_title(electronic_references, [в, "базы данных, информационно-справочные и поисковые системы:"]).
    section_title(document_references, [г, "нормативные документы:"]).
    section_title(current_control_examples, "Примеры оценочных средств текущего контроля").
    section_title(intermediate_control_questions, "Список вопросов для промежуточной аттестации:").
    section_title(intermediate_control_examples, "Примеры оценочных средств для промежуточной аттестации:").
    section_title(author, "Разработчик:").

:- end_category.
