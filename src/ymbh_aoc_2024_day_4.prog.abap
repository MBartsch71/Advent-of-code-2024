REPORT ymbh_aoc_2024_day_4.

CLASS xmas_search DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS set_input IMPORTING input TYPE stringtab.

    METHODS find_xmas_amount_horizontal.
    METHODS get_occurences RETURNING VALUE(result) TYPE i.
    METHODS find_xmas_amount_vertical.
    METHODS find_xmas_amount_diagonal.
    METHODS process_input.

  PRIVATE SECTION.
    CONSTANTS regex_horizontal TYPE string VALUE '(?=XMAS|SAMX)'.
    CONSTANTS regex_diagonal TYPE string VALUE 'X....M....A....S|...X..M..A..S...|S....A....M....X|...S..A..M..X...'.

    DATA input TYPE stringtab.
    DATA occurences TYPE i.
    METHODS find_xmas_diagonal_ltbr.
    METHODS find_xmas_diagonal_rtbl.

ENDCLASS.

CLASS xmas_search IMPLEMENTATION.

  METHOD set_input.
    me->input = input.
  ENDMETHOD.

  METHOD find_xmas_amount_horizontal.
    LOOP AT input REFERENCE INTO DATA(line).
      FIND ALL OCCURRENCES OF PCRE regex_horizontal IN line->* RESULTS DATA(matches).
      occurences = occurences + lines( matches ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_occurences.
    result = me->occurences.
  ENDMETHOD.

  METHOD find_xmas_amount_vertical.
    DATA(index_from) = 1.
    DATA(table_height) = lines( input ) - 3.
    DATA(table_width) = strlen( input[ 1 ] ).
    DO table_height TIMES.
      DATA(part_table) = VALUE stringtab( FOR i = index_From THEN i + 1 UNTIL i = index_from + 4
                                            ( input[ i ] ) ).
      DATA(offset) = 0.
      DO table_width TIMES.
        DATA(expression) = |{ substring( val = part_table[ 1 ] off = offset len = 1 ) }| &&
                           |{ substring( val = part_table[ 2 ] off = offset len = 1 ) }| &&
                           |{ substring( val = part_table[ 3 ] off = offset len = 1 ) }| &&
                           |{ substring( val = part_table[ 4 ] off = offset len = 1 ) }|.
        FIND ALL OCCURRENCES OF PCRE regex_horizontal IN expression RESULTS DATA(matches).
        occurences = occurences + lines( matches ).
        offset += 1.
      ENDDO.
      index_From += 1.
    ENDDO.
  ENDMETHOD.

  METHOD find_xmas_amount_diagonal.
    find_xmas_diagonal_ltbr( ).
    find_xmas_diagonal_rtbl( ).
  ENDMETHOD.

  METHOD find_xmas_diagonal_ltbr.
    DATA(index_from) = 1.
    DATA(table_height) = lines( input ) - 3.
    DATA(table_width) = strlen( input[ 1 ] ) - 3.

    DO table_height TIMES.
      DATA(part_table) = VALUE stringtab( FOR i = index_from THEN i + 1 UNTIL i = index_from + 4
                                           ( input[ i ] ) ).
      DATA(offset) = 0.
      DO table_width TIMES.
        DATA(expression) = |{ substring( val = part_table[ 1 ] off = offset len = 1 ) }| &&
                           |{ substring( val = part_table[ 2 ] off = offset + 1 len = 1 ) }| &&
                           |{ substring( val = part_table[ 3 ] off = offset + 2 len = 1 ) }| &&
                           |{ substring( val = part_table[ 4 ] off = offset + 3 len = 1 ) }|.
        IF expression = |XMAS| OR expression = |SAMX|.
          occurences += 1.
        ENDIF.
        offset += 1.
      ENDDO.
      index_from += 1.
    ENDDO.
  ENDMETHOD.

  METHOD find_xmas_diagonal_rtbl.
    DATA(index_from) = 1.
    DATA(table_height) = lines( input ) - 3.
    DATA(table_width) = strlen( input[ 1 ] ) - 3.

    DO table_height TIMES.
      DATA(part_table) = VALUE stringtab( FOR i = index_from THEN i + 1 UNTIL i = index_from + 4
                                           ( input[ i ] ) ).
      DATA(offset) = 0.
      DO table_width TIMES.
        DATA(expression) = |{ substring( val = part_table[ 1 ] off = offset + 3 len = 1 ) }| &&
                           |{ substring( val = part_table[ 2 ] off = offset + 2 len = 1 ) }| &&
                           |{ substring( val = part_table[ 3 ] off = offset + 1 len = 1 ) }| &&
                           |{ substring( val = part_table[ 4 ] off = offset len = 1 ) }|.
        IF expression = |XMAS| OR expression = |SAMX|.
          occurences += 1.
        ENDIF.
        offset += 1.
      ENDDO.
      index_from += 1.
    ENDDO.
  ENDMETHOD.

  METHOD process_input.
    find_xmas_amount_horizontal( ).
    find_xmas_amount_vertical( ).
    find_xmas_amount_diagonal( ).
  ENDMETHOD.

ENDCLASS.


CLASS test_search DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO xmas_search.

    METHODS setup.
    METHODS can_create_object FOR TESTING.
    METHODS search_pattern_horizontal FOR TESTING.
    METHODS search_pattern_vertical FOR TESTING.
    METHODS serch_pattern_diagonal FOR TESTING.

    METHODS combine_all_patterns FOR TESTING.
ENDCLASS.

CLASS test_search IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
  ENDMETHOD.

  METHOD can_create_object.
    DATA(cut) = NEW xmas_search( ).
  ENDMETHOD.

  METHOD search_pattern_horizontal.
    DATA(input) = VALUE stringtab( ( |XMASXXSAMXSMXXMASASASXMASXXSAMX| ) ).
    cut->set_input( input ).
    cut->find_xmas_amount_horizontal( ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = cut->get_occurences( ) ).
  ENDMETHOD.

  METHOD search_pattern_vertical.
    DATA(input) = VALUE stringtab( ( |MAXSXMASXASMXASMSAXMSAXMSAXMSS| )
                                   ( |AMMMAMXASMXASMXASMXASMXASMXASM| )
                                   ( |XAAXSXMASXASMXASMXMSMXSAMXSAMX| )
                                   ( |SSSXASMXASMXSMMXSAAXSAMSAMXSAM| )
                                   ( |XASMXSAMXSAMXAAMASSMXASMXASMXA| )
                                   ( |SMXASMXSAMXSASXXMMXSASXSAMXXSX| )
                                   ( |XASSXSAMXSAMXSAMXXSAMASAMXAMMM| )
                                   ( |ASMXSAMXSAMXSAMSAMXASMXASMXASA| )
                                   ( |XSAMXSAMXSAMXXSAMXASMXASMXSSMS| ) ).
    cut->set_input( input ).
    cut->find_xmas_amount_vertical( ).
    cl_abap_unit_assert=>assert_equals( exp = 8 act = cut->get_occurences( ) ).
  ENDMETHOD.

  METHOD serch_pattern_diagonal.
    DATA(input) = VALUE stringtab( ( |XXMASSMXSXMASSMS| )
                                   ( |AMSSMAMAAASSMAAA| )
                                   ( |XMAMSAXMXMMMSMXM| )
                                   ( |MXMSSMASMXMXXASX| )
                                   ( |XXMASSMXSXMASSMS| )
                                   ( |AMSSMAMAAASSMAAA| )
                                   ( |XMAMSAXMXMMMSMXM| )
                                   ( |MXMSSMASMXMXMASX| ) ).
    cut->set_input( input ).
    cut->find_xmas_amount_diagonal( ).
    cl_abap_unit_assert=>assert_equals( exp = 8 act = cut->get_occurences( )  ).
  ENDMETHOD.

  METHOD combine_all_patterns.
    DATA(input) = VALUE stringtab( ( |MMMSXXMASM| )
                                   ( |MSAMXMSMSA| )
                                   ( |AMXSXMAAMM| )
                                   ( |MSAMASMSMX| )
                                   ( |XMASAMXAMM| )
                                   ( |XXAMMXXAMA| )
                                   ( |SMSMSASXSS| )
                                   ( |SAXAMASAAA| )
                                   ( |MAMMMXMMMM| )
                                   ( |MXMXAXMASX| ) ).
    cut->set_input( input ).
    cut->process_input( ).
    cl_abap_unit_assert=>assert_equals( exp = 18 act = cut->get_occurences( ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(xmas_finder) = NEW xmas_search( ).
  xmas_finder->set_input( input_data ).
  xmas_finder->process_input( ).

  WRITE /: |The result of part 1 is: { xmas_finder->get_occurences( ) }|.
  WRITE /: |The result of part 2 is: tbd|.
