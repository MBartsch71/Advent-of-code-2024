REPORT ymbh_aoc_2024_day_2.

CLASS safety_report DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF line_difference_item,
             item                      TYPE i,
             difference_to_predecessor TYPE i,
             diff_ok                   TYPE abap_bool,
           END OF line_difference_item.
    TYPES line_differences TYPE STANDARD TABLE OF line_difference_item WITH EMPTY KEY.

    TYPES: BEGIN OF table_Check_item,
             tableline  TYPE i,
             entry_safe TYPE abap_bool,
           END OF table_check_item.
    TYPES table_checks TYPE STANDARD TABLE OF table_Check_item WITH EMPTY KEY.

    types input_table_int TYPE STANDARD TABLE OF i with DEFAULT KEY.

    METHODS line_is_consecutively
      IMPORTING
        input_data    TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS constructor IMPORTING input_data TYPE stringtab.
    METHODS line_has_right_differences
      IMPORTING
        input_data    TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.
    METHODS analyze_data.
    METHODS get_analysis_result
      RETURNING
        VALUE(result) TYPE table_checks.
    METHODS get_amount_of_right_reports
      RETURNING
        VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA input_data TYPE stringtab.
    DATA analysis_result TYPE table_checks.

    METHODS check_difference
      IMPORTING
                item          TYPE i
                check_table   TYPE line_differences
                index         TYPE i
      RETURNING VALUE(result) TYPE i.
    METHODS check_line
      IMPORTING
        line          TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS safety_report IMPLEMENTATION.

  METHOD constructor.
    me->input_data = input_data.
  ENDMETHOD.


  METHOD line_is_consecutively.
    data input_table_numbers type input_table_int.

    SPLIT input_data AT space INTO TABLE DATA(input_table).

    DELETE input_table WHERE table_line = ''.
    input_table_numbers = input_table.
    DATA(check_table_asc) = input_Table_numbers.
    DATA(check_table_desc) = input_table_numbers.
    SORT check_table_asc ASCENDING.
    SORT check_table_desc DESCENDING.

    result = xsdbool( check_table_asc = input_table_numbers OR
                      check_table_desc = input_table_numbers ).

  ENDMETHOD.

  METHOD line_has_right_differences.
    DATA line_diff TYPE line_differences.

    SPLIT input_data AT space INTO TABLE DATA(input_table).

    line_diff = VALUE #( FOR line IN input_table
                           INDEX INTO tab_index
                           LET difference = check_difference( item = CONV #( line )
                                                              check_table = line_diff
                                                              index = tab_index )
                           IN
                           ( item = line
                             difference_to_predecessor = difference
                             diff_ok = COND #( WHEN difference >= 1 AND difference <= 3
                                                        THEN abap_true ) ) ).

    result = xsdbool( NOT line_exists( line_diff[ diff_ok = abap_false ] ) ).
  ENDMETHOD.

  METHOD check_difference.
    IF index = 1.
      result = 1.
      RETURN.
    ENDIF.

    DATA(predecessor) = check_table[ index - 1 ]-item.
    result = COND #( WHEN abs( item - predecessor ) > 0
                        THEN abs( item - predecessor )
                        ELSE -1 ).
  ENDMETHOD.

  METHOD analyze_data.
    analysis_result = VALUE #( FOR line IN input_data
                                    INDEX INTO line_index
                                    ( tableline = line_index
                                      entry_safe = check_line( line ) ) ).
  ENDMETHOD.

  METHOD get_analysis_result.
    result = analysis_result.
  ENDMETHOD.

  METHOD check_line.
    result = xsdbool( line_is_consecutively( line ) AND
                      line_has_right_differences( line ) ).
  ENDMETHOD.

  METHOD get_amount_of_right_reports.
    result = REDUCE #( INIT sum = 0
                       FOR line IN analysis_result
                       NEXT sum = COND #( WHEN line-entry_safe = abap_true
                                            THEN sum + 1
                                            ELSE sum ) ).
  ENDMETHOD.

ENDCLASS.


CLASS test_safety_reports DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO safety_report.

    METHODS setup.
    METHODS can_create_object FOR TESTING.
    METHODS entries_are_consecutively FOR TESTING.
    METHODS entries_are_not_consecutively FOR TESTING.
    METHODS entries_right_differences FOR TESTING.
    METHODS entries_wrong_differences FOR TESTING.
    METHODS input_table_with_result FOR TESTING.
    METHODS right_amount_of_safe_reports FOR TESTING.
    methods single_entry_check for testing.
ENDCLASS.

CLASS test_safety_reports IMPLEMENTATION.

  METHOD setup.
    DATA(input_Data) = VALUE stringtab( ( |7 6 4 2 1| )
                                        ( |1 2 7 8 9| )
                                        ( |9 7 6 2 1| )
                                        ( |1 3 2 4 5| )
                                        ( |8 6 4 4 1| )
                                        ( |1 3 6 7 9| ) ).
    cut = NEW safety_report( input_data ).
  ENDMETHOD.

  METHOD can_create_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD entries_are_consecutively.
    DATA(input_data) = |7 6 4 2 1|.
    cl_abap_unit_assert=>assert_true( act = cut->line_is_consecutively( input_data ) ).
  ENDMETHOD.

  METHOD entries_are_not_consecutively.
    DATA(input_data) = |1 3 2 4 5|.
    cl_abap_unit_assert=>assert_false( act = cut->line_is_consecutively( input_data ) ).
  ENDMETHOD.

  METHOD entries_right_differences.
    DATA(input_data) = |7 6 4 2 1|.
    cl_abap_unit_assert=>assert_true( act = cut->line_has_right_differences( input_data )  ).
  ENDMETHOD.

  METHOD entries_wrong_differences.
    DATA(input_data) = |1 2 7 8 9|.
    cl_abap_unit_assert=>assert_false( act = cut->line_has_right_differences( input_data )  ).
  ENDMETHOD.

  METHOD input_table_with_result.
    DATA(expected_data) = VALUE safety_report=>table_checks( ( tableline = 1 entry_safe = abap_true )
                                                             ( tableline = 2 entry_safe = abap_false )
                                                             ( tableline = 3 entry_safe = abap_false )
                                                             ( tableline = 4 entry_safe = abap_false )
                                                             ( tableline = 5 entry_safe = abap_false )
                                                             ( tableline = 6 entry_safe = abap_true ) ).
    cut->analyze_data( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_data act = cut->get_analysis_result( ) ).

  ENDMETHOD.

  METHOD right_amount_of_safe_reports.
    cut->analyze_data( ).
    cl_abap_unit_assert=>assert_equals( exp = 2 act = cut->get_amount_of_right_reports( ) ).
  ENDMETHOD.

  METHOD single_entry_check.
    data(input_Data) = value stringtab( ( |14 12 10 7 4| ) ).
    data(cut) = new safety_report( input_data ).
    cut->analyze_data( ).
    cl_abap_unit_assert=>assert_equals( exp = 1 act = cut->get_amount_of_right_reports( )  ).

  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(analyzer) = NEW safety_report( input_data ).
  analyzer->analyze_data( ).

  WRITE /: |The result of part 1 is: { analyzer->get_amount_of_right_reports( ) }|.
  WRITE /: |The result of part 2 is: tbd|.
