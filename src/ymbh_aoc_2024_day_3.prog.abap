REPORT ymbh_aoc_2024_day_3.

CLASS expression_finder DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF expression_item,
             whole_string TYPE string,
             number_one   TYPE i,
             number_two   TYPE i,
           END OF expression_item.
    TYPES expressions TYPE STANDARD TABLE OF expression_item WITH EMPTY KEY.

    METHODS constructor IMPORTING input TYPE string.

    METHODS process_input.
    METHODS get_expression_table RETURNING VALUE(result) TYPE expressions.
    METHODS calculate_expression
      IMPORTING
        expressions   TYPE expression_finder=>expressions
      RETURNING
        VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA input TYPE string.
    DATA expressions_tab TYPE expressions.

    DATA regex TYPE string VALUE 'mul\((\d{1,3}),(\d{1,3})\)'.

ENDCLASS.

CLASS expression_finder IMPLEMENTATION.

  METHOD constructor.
    me->input = input.
  ENDMETHOD.

  METHOD process_input.
    FIND ALL OCCURRENCES OF PCRE regex IN input RESULTS DATA(matches).

    expressions_tab = VALUE #( FOR match IN matches
                                ( whole_string = substring( val = input off = match-offset len = match-length )
                                  number_one = CONV #( substring( val = input off = match-submatches[ 1 ]-offset len = match-submatches[ 1 ]-length ) )
                                  number_two = CONV #( substring( val = input off = match-submatches[ 2 ]-offset len = match-submatches[ 2 ]-length ) ) ) ).
  ENDMETHOD.

  METHOD get_expression_table.
    result = expressions_tab.
  ENDMETHOD.

  METHOD calculate_expression.
    result = REDUCE #( INIT sum = 0
                       FOR line IN expressions
                       NEXT sum = sum + ( line-number_one * line-number_two ) ).
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
ENDCLASS.

CLASS test_expression_finder IMPLEMENTATION.

  METHOD setup.
    DATA(input) = |xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))|.
    cut  = NEW #( input ).
  ENDMETHOD.

  METHOD find_the_expressions.
    DATA(expected_values) = VALUE expression_finder=>expressions( ( whole_string = |mul(2,4)|  number_one = 2  number_two = 4 )
                                                                  ( whole_string = |mul(5,5)|  number_one = 5  number_two = 5 )
                                                                  ( whole_string = |mul(11,8)| number_one = 11 number_two = 8 )
                                                                  ( whole_string = |mul(8,5)|  number_one = 8  number_two = 5 ) ).
    cut->process_input( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_expression_table( )  ).
  ENDMETHOD.

  METHOD get_result.
    cut->process_input( ).
    cl_abap_unit_assert=>assert_equals( exp = 161 act = cut->calculate_expression( cut->get_expression_table( ) )  ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_string( ).
  DATA(expression_builder) = NEW expression_finder( input_data ).

  expression_builder->process_input( ).
  WRITE /: |The result of part 1 is: { expression_builder->calculate_expression( expressions = expression_builder->get_expression_table( ) ) }|.
  WRITE /: |The result of part 2 is: tbd|.
