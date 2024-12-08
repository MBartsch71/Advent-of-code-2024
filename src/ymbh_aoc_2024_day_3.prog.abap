REPORT ymbh_aoc_2024_day_3.

CLASS expression_finder DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF expression_item,
             whole_string TYPE string,
             number_one   TYPE i,
             number_two   TYPE i,
             start_offset TYPE i,
             length       TYPE i,
           END OF expression_item.
    TYPES expressions TYPE STANDARD TABLE OF expression_item WITH EMPTY KEY.

    METHODS constructor IMPORTING input TYPE string.

    METHODS process_input.

    METHODS get_expression_table RETURNING VALUE(result) TYPE expressions.

    METHODS calculate_expression IMPORTING expressions   TYPE expression_finder=>expressions
                                 RETURNING VALUE(result) TYPE i.

    METHODS find_instructions RETURNING VALUE(result) TYPE expressions.

    METHODS calculate_with_instructions IMPORTING instructions  TYPE expression_finder=>expressions
                                        RETURNING VALUE(result) TYPE i.
    METHODS reset_result.

  PRIVATE SECTION.
    DATA input TYPE string.
    DATA expressions_tab TYPE expressions.

    CONSTANTS regex TYPE string VALUE 'mul\((\d{1,3}),(\d{1,3})\)'.
    CONSTANTS regex_instruction TYPE string VALUE 'don['']t\(\)|do\(\)'.

    METHODS determine_end_offset IMPORTING index         TYPE i
                                           table         TYPE expressions
                                 RETURNING VALUE(result) TYPE i.

    METHODS read_main_expression IMPORTING match         TYPE match_result
                                 RETURNING VALUE(result) TYPE string.

    METHODS get_first_factor IMPORTING match         TYPE match_result
                             RETURNING VALUE(result) TYPE i.

    METHODS get_second_factor IMPORTING match         TYPE match_result
                              RETURNING VALUE(result) TYPE string.
ENDCLASS.

CLASS expression_finder IMPLEMENTATION.

  METHOD constructor.
    me->input = input.
  ENDMETHOD.

  METHOD process_input.
    FIND ALL OCCURRENCES OF PCRE regex IN input RESULTS DATA(matches).

    expressions_tab = VALUE #( FOR match IN matches
                                ( whole_string = read_main_expression( match )
                                  number_one = get_first_factor( match )
                                  number_two = get_second_factor( match )
                                  start_offset = match-offset ) ).
  ENDMETHOD.

  METHOD get_expression_table.
    result = expressions_tab.
  ENDMETHOD.

  METHOD calculate_expression.
    result = REDUCE #( INIT sum = 0
                       FOR line IN expressions
                       NEXT sum = sum + ( line-number_one * line-number_two ) ).
  ENDMETHOD.

  METHOD find_instructions.
    FIND ALL OCCURRENCES OF PCRE regex_instruction IN input RESULTS DATA(matches).

    DATA(temp_result) = VALUE expressions( FOR match IN matches
                                            ( whole_string = substring( val = input off = match-offset len = match-length )
                                              start_offset = match-offset ) ).

    DATA(kk) = strlen( input ).
    result = VALUE #( FOR line IN temp_result
                        INDEX INTO tab_index
                        LET length = determine_end_offset( index = tab_index
                                                           table = temp_result )
                        IN
                        ( whole_string = line-whole_string
                          number_one = line-number_one
                          number_two = line-number_two
                          start_offset = line-start_offset
                          length = COND #( WHEN length = -1
                                             THEN strlen( input ) - line-start_offset
                                             ELSE length ) ) ).

    IF result[ 1 ]-whole_string = |don't()|.
      DATA(start_line) = VALUE expression_item( whole_string = |do()| start_offset = 0 length = result[ 1 ]-start_offset ).
      INSERT start_line INTO result INDEX 1.
    ENDIF.

  ENDMETHOD.

  METHOD determine_end_offset.
    IF line_exists( table[ index + 1 ] ).
      result = table[ index + 1  ]-start_offset - table[ index ]-start_offset.
    ELSE.
      result = -1.
    ENDIF.
  ENDMETHOD.

  METHOD calculate_with_instructions.
    LOOP AT instructions REFERENCE INTO DATA(inst) WHERE ( whole_string = |do()| ).

      DATA(part_string) = substring( val = input off = inst->start_offset len = inst->length ).
      FIND ALL OCCURRENCES OF PCRE regex IN part_string RESULTS DATA(matches).

      expressions_tab = VALUE #( BASE expressions_tab
                                 FOR match IN matches
                                    ( whole_string = substring( val = part_string off = match-offset len = match-length )
                                      number_one = CONV #( substring( val = part_string off = match-submatches[ 1 ]-offset len = match-submatches[ 1 ]-length ) )
                                      number_two = CONV #( substring( val = part_string off = match-submatches[ 2 ]-offset len = match-submatches[ 2 ]-length ) )
                                      start_offset = match-offset ) ).
    ENDLOOP.

  ENDMETHOD.

  METHOD reset_result.
    CLEAR expressions_tab.
  ENDMETHOD.

  METHOD read_main_expression.
    result = substring( val = input off = match-offset len = match-length ).
  ENDMETHOD.

  METHOD get_first_factor.
    result = substring( val = input off = match-submatches[ 1 ]-offset len = match-submatches[ 1 ]-length ).
  ENDMETHOD.

  METHOD get_second_factor.
    result = substring( val = input off = match-submatches[ 2 ]-offset len = match-submatches[ 2 ]-length ).
  ENDMETHOD.

ENDCLASS.


CLASS test_expression_finder DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO expression_finder.

    METHODS setup.

    METHODS find_the_expressions FOR TESTING.
    METHODS get_result FOR TESTING.
    METHODS find_new_do_dont_expressions FOR TESTING.
    METHODS new_calculation_expressions FOR TESTING.
ENDCLASS.

CLASS test_expression_finder IMPLEMENTATION.

  METHOD setup.
    DATA(input) = |xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|.
    cut  = NEW #( input ).
  ENDMETHOD.

  METHOD find_the_expressions.
    DATA(expected_values) = VALUE expression_finder=>expressions( ( whole_string = |mul(2,4)|  number_one = 2  number_two = 4 start_offset = 1 )
                                                                  ( whole_string = |mul(5,5)|  number_one = 5  number_two = 5 start_offset = 29 )
                                                                  ( whole_string = |mul(11,8)| number_one = 11 number_two = 8 start_offset = 53 )
                                                                  ( whole_string = |mul(8,5)|  number_one = 8  number_two = 5 start_offset = 62 ) ).
    cut->process_input( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_expression_table( )  ).
  ENDMETHOD.

  METHOD get_result.
    cut->process_input( ).
    cl_abap_unit_assert=>assert_equals( exp = 161 act = cut->calculate_expression( cut->get_expression_table( ) )  ).
  ENDMETHOD.

  METHOD find_new_do_dont_expressions.
    DATA(input) = |xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|.
    cut = NEW #( input ).
    DATA(expected_values) = VALUE expression_finder=>expressions( ( whole_string = |do()|    start_offset = 0  length = 20 )
                                                                  ( whole_string = |don't()| start_offset = 20 length = 39 )
                                                                  ( whole_string = |do()|    start_offset = 59 length = 14 ) ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->find_instructions( ) ).
  ENDMETHOD.

  METHOD new_calculation_expressions.
    DATA(input) = |xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))|.
    cut = NEW #( input ).
    DATA(expected_values) = VALUE expression_finder=>expressions( ( whole_string = |mul(2,4)| number_one = 2 number_two = 4 start_offset = 1 )
                                                                  ( whole_string = |mul(8,5)| number_one = 8 number_two = 5 start_offset = 62 ) ).
    DATA(instructions) = cut->find_instructions(  ).
    cut->calculate_with_instructions( instructions ).
    cl_abap_unit_assert=>assert_equals( exp = 48 act = cut->calculate_expression( cut->get_expression_table( ) ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_string( ).
  DATA(expression_builder) = NEW expression_finder( input_data ).

  expression_builder->process_input( ).
  WRITE /: |The result of part 1 is: { expression_builder->calculate_expression( expression_builder->get_expression_table( ) ) }|.

  expression_builder->reset_result( ).
  expression_builder->calculate_with_instructions( expression_builder->find_instructions( ) ).
  WRITE /: |The result of part 2 is: { expression_builder->calculate_expression( expression_builder->get_expression_table( ) ) }|.
