REPORT ymbh_aoc_2024_day_7.

CLASS bridge_repair DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: calculation_terms TYPE STANDARD TABLE OF p WITH EMPTY KEY.
    TYPES: BEGIN OF calculation_pair,
             result    TYPE p LENGTH 16 DECIMALS 0,
             terms     TYPE calculation_terms,
             result_ok TYPE abap_bool,
           END OF calculation_pair.
    TYPES calculation_pairs TYPE STANDARD TABLE OF calculation_pair WITH EMPTY KEY.
    TYPES: BEGIN OF operation_matcher,
             binary_string TYPE string,
             operation     TYPE string,
           END OF operation_matcher.
    TYPES operations_matcher TYPE TABLE OF operation_matcher WITH EMPTY KEY.

    METHODS constructor IMPORTING input TYPE stringtab.

    METHODS get_calculation_pairs RETURNING VALUE(result) TYPE calculation_pair.

    METHODS calculations_are_valid RETURNING VALUE(result) TYPE calculation_pairs.

    METHODS get_sum_of_right_results IMPORTING calculation_pairs TYPE bridge_repair=>calculation_pairs
                                     RETURNING VALUE(result)     TYPE decfloat16.

  PRIVATE SECTION.
    DATA calc_pair TYPE calculation_pair.
    DATA calc_pairs TYPE calculation_pairs.
    DATA binary_tool_to_bin TYPE REF TO zif_to_binary.
    DATA operations TYPE operations_matcher.
    DATA binary_tool_to_dec TYPE REF TO zif_from_binary.

    METHODS determine_operands IMPORTING calc_pair     TYPE bridge_repair=>calculation_pair
                               RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS bridge_repair IMPLEMENTATION.

  METHOD constructor.
    DATA equation_terms TYPE calculation_terms.

    binary_tool_to_bin = NEW zcl_integer_to_binary( ).

    operations = VALUE #( ( binary_string = |0| operation = |+| )
                          ( binary_string = |1| operation = |*| ) ).
    LOOP AT input REFERENCE INTO DATA(input_line).
      SPLIT input_line->* AT ':' INTO DATA(result) DATA(terms).
      SPLIT condense( terms ) AT space INTO TABLE DATA(c_terms).
      calc_pairs = VALUE #( BASE calc_pairs ( result = result terms = c_terms ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_calculation_pairs.
    result = calc_pair.
  ENDMETHOD.

  METHOD calculations_are_valid.
    DATA bin TYPE string.
    DATA bin_chars TYPE string.
    DATA calc_table TYPE STANDARD TABLE OF decfloat16.
    DATA calc_table_offset TYPE i.

    LOOP AT calc_pairs REFERENCE INTO DATA(calc_pair).
      DATA(operands_needed) = determine_operands( calc_pair->* ).
      DATA(current_value) = 0.

      bin_chars = |{ 1 WIDTH = operands_needed + 1 PAD = '0' }|.
      binary_tool_to_dec = NEW zcl_binary_to_integer( ).
      DATA(counter) = binary_tool_to_dec->convert( bin_chars ).
      ASSIGN counter->* TO FIELD-SYMBOL(<counter>).
      WHILE current_value <= <counter>.
        IF current_value = 0.
          bin = |{ 0 WIDTH = operands_needed PAD = '0' }|.
        ELSE.
          bin = |{ binary_tool_to_bin->convert( REF #( current_value ) ) ALIGN = RIGHT WIDTH = operands_needed PAD = '0' }|.
        ENDIF.

        calc_table = calc_pair->terms.
        calc_table_offset = 3.
        DATA(terms_count) = lines( calc_pair->terms ).
        DATA(offset) = 0.
        DO.
          DATA(operation) = operations[ binary_string = substring( val = bin off = offset len = 1 ) ]-operation.

          DATA(calc_result) = COND decfloat16(  WHEN operation = '+' THEN calc_table[ 1 ] + calc_table[ 2 ]
                                       WHEN operation = '*' THEN calc_table[ 1 ] * calc_table[ 2 ] ).
          calc_table = VALUE #( ( calc_result ) ).
          TRY.
              calc_table = VALUE #( BASE calc_table
                                      FOR i = calc_table_offset THEN i + 1 UNTIL i = terms_count
                                      ( calc_pair->terms[ i ] ) ).
            CATCH cx_sy_itab_line_not_found.

          ENDTRY.
          calc_table_offset += 1.
          offset += 1.
          IF lines( calc_table ) = 1.
            EXIT.
          ENDIF.
        ENDDO.

        IF calc_table[ 1 ] = calc_pair->result.
          calc_pair->result_ok = abap_true.
          EXIT.
        ENDIF.
        current_value += 1.
      ENDWHILE.
    ENDLOOP.
    result = calc_pairs.
  ENDMETHOD.

  METHOD determine_operands.
    result = lines( calc_pair-terms ).
  ENDMETHOD.


  METHOD get_sum_of_right_results.
    DATA temp_sum TYPE p LENGTH 16 DECIMALS 0 VALUE 0.
    result = REDUCE #( INIT sum = temp_sum
                       FOR line IN calculation_pairs
                       NEXT sum = COND #(  WHEN line-result_ok = abap_true
                                            THEN sum  + line-result
                                            ELSE sum ) ).
  ENDMETHOD.

ENDCLASS.


CLASS test_bridge_repair DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    TYPES: calculation_terms TYPE STANDARD TABLE OF int8 WITH EMPTY KEY.
    TYPES: BEGIN OF calculation_pair,
             result    TYPE int8,
             terms     TYPE calculation_terms,
             result_ok TYPE abap_bool,
           END OF calculation_pair.
    TYPES: calculation_pairs TYPE STANDARD TABLE OF calculation_pair WITH EMPTY KEY.
    METHODS check_calculation_can_be_right FOR TESTING.
    METHODS check_3_term_calculation_is_ok FOR TESTING.
    METHODS check_13_terms_calc_ok FOR TESTING.
    METHODS check_all_mult_terms FOR TESTING.

    METHODS get_sum_of_right_results FOR TESTING.
ENDCLASS.

CLASS test_bridge_repair IMPLEMENTATION.

  METHOD check_calculation_can_be_right.
    DATA(cut) = NEW bridge_repair( VALUE stringtab( ( |190: 10 19| ) ) ).
    DATA(result) = cut->calculations_are_valid( ).
    cl_abap_unit_assert=>assert_true( act = result[ 1 ]-result_ok ).
  ENDMETHOD.

  METHOD check_3_term_calculation_is_ok.
    DATA(cut) = NEW bridge_repair( VALUE stringtab( ( |3267: 81 40 27| ) ) ).
    DATA(result) = cut->calculations_are_valid( ).
    cl_abap_unit_assert=>assert_true( act = result[ 1 ]-result_ok ).
  ENDMETHOD.

  METHOD get_sum_of_right_results.
    DATA(cut) = NEW bridge_repair( VALUE stringtab( ( |190: 10 19| )
                                                    ( |3267: 81 40 27| )
                                                    ( |83: 17 5| )
                                                    ( |156: 15 6| )
                                                    ( |7290: 6 8 6 15| )
                                                    ( |161011: 16 10 13| )
                                                    ( |192: 17 8 14| )
                                                    ( |21037: 9 7 18 13| )
                                                    ( |292: 11 6 16 20| ) ) ).

    DATA(calculation_pairs) = cut->calculations_are_valid( ).
    cl_abap_unit_assert=>assert_equals( exp = 3749 act = cut->get_sum_of_right_results( calculation_pairs )  ).
  ENDMETHOD.

  METHOD check_13_terms_calc_ok.
    DATA(cut) = NEW bridge_repair( VALUE stringtab( ( |49260: 2 3 4 6 6 7 3 3 5 4 5 6 5 5| ) ) ).
    DATA(calculation_pairs) = cut->calculations_are_valid( ).
    cl_abap_unit_assert=>assert_equals( exp = 49260 act = cut->get_sum_of_right_results( calculation_pairs ) ).
  ENDMETHOD.

  METHOD check_all_mult_terms.
    DATA(cut) = NEW bridge_repair( VALUE stringtab( ( |34560: 3 5 6 2 4 2 8 3| ) ) ).
    DATA(calculation_pairs) = cut->calculations_are_valid( ).
    cl_abap_unit_assert=>assert_equals( exp = 34560 act = cut->get_sum_of_right_results( calculation_pairs ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(bridge_repair) = NEW bridge_repair( input_data ).
  DATA(calculation_pairs) = bridge_repair->calculations_are_valid( ).

  WRITE /: |The result of part 1 is: { bridge_repair->get_sum_of_right_results( calculation_pairs ) }|.
  WRITE /: |The result of part 2 is: tbd|.
