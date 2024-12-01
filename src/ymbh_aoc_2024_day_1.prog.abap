REPORT ymbh_aoc_2024_day_1.

INTERFACE historian_hysteria.
  TYPES: BEGIN OF location_list_item,
           left_list_value  TYPE i,
           right_list_value TYPE i,
           distance         TYPE i,
         END OF location_list_item.
  TYPES location_list TYPE STANDARD TABLE OF location_list_item WITH EMPTY KEY.
  TYPES: BEGIN OF similiarity_list_item,
           left_list_value       TYPE i,
           appears_in_right_list TYPE i,
         END OF similiarity_list_item.
  TYPES similarity_list TYPE STANDARD TABLE OF similiarity_list_item WITH EMPTY KEY.
ENDINTERFACE.

CLASS simple_list_comparison DEFINITION FINAL.

  PUBLIC SECTION.
    METHODS constructor IMPORTING input_data TYPE stringtab.

    METHODS calculate_total_distance RETURNING VALUE(result) TYPE i.
    METHODS calculate_similarity_score RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA left_list TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
    DATA right_list TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.

    DATA distances_list TYPE historian_hysteria=>location_list.
    DATA similarities TYPE historian_hysteria=>similarity_list.

    METHODS split_list importing input_data type stringtab.
    METHODS build_distances_lists.
    METHODS build_similarity_list.

    METHODS check_distance IMPORTING left_item     TYPE i
                                     right_item    TYPE i
                           RETURNING VALUE(result) TYPE i.
ENDCLASS.

CLASS simple_list_comparison IMPLEMENTATION.

  METHOD constructor.
    split_list( input_data ).
  ENDMETHOD.

  METHOD calculate_total_distance.
    build_distances_lists( ).
    result = REDUCE #( INIT sum = 0
                       FOR line IN distances_list
                       NEXT sum = sum + line-distance ).
  ENDMETHOD.

  METHOD calculate_similarity_score.
    build_similarity_list( ).
    result = REDUCE #( INIT score = 0
                       FOR line IN similarities
                       NEXT score = score + line-appears_in_right_list ).
  ENDMETHOD.

  METHOD split_list.
    LOOP AT input_data REFERENCE INTO DATA(line).
      SPLIT line->* AT space INTO TABLE DATA(values).
      left_list = VALUE #( BASE left_List ( values[ 1 ] ) ).
      right_list = VALUE #( BASE right_list ( values[ lines( values ) ] ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD build_distances_lists.
    DATA(list_length) = lines( left_list ).
    DATA(loop_index) = 1.

    DO list_length TIMES.
      distances_list = VALUE #( BASE distances_list ( left_list_value = left_list[ loop_index ]
                                                    right_list_value = right_list[ loop_index ]
                                                    distance = check_distance( left_item = CONV #( left_list[ loop_index ] )
                                                                               right_item = CONV #( right_list[ loop_index ] ) ) ) ).
      loop_index += 1.
    ENDDO.
  ENDMETHOD.

  METHOD build_similarity_list.
    DATA(list_length) = lines( left_list ).
    DATA(loop_index) = 1.

    DO list_length TIMES.
      DATA(left_item) = left_list[ loop_index ].
      DATA(occurences) = REDUCE #( INIT sum = 0
                                   FOR line IN right_list
                                   NEXT sum = COND #( WHEN left_item = line THEN sum + 1
                                                      ELSE sum ) ).
      similarities = VALUE #( BASE similarities ( left_list_value = left_item appears_in_right_list = occurences * left_item ) ).
      loop_index += 1.
    ENDDO.

  ENDMETHOD.

  METHOD check_distance.
    result = abs( left_item - right_item ).
  ENDMETHOD.

ENDCLASS.

CLASS test_list_comparison DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA  cut TYPE REF TO simple_list_comparison.

    METHODS setup.

    METHODS get_valid_total_distance FOR TESTING.
    METHODS get_similarity_Score FOR TESTING.

ENDCLASS.

CLASS test_list_comparison IMPLEMENTATION.

  METHOD setup.
    DATA(input_data) = VALUE stringtab( ( |3   4| )
                                        ( |4   3| )
                                        ( |2   5| )
                                        ( |1   3| )
                                        ( |3   9| )
                                        ( |3   3| ) ).
    cut = NEW simple_list_comparison(  input_data ).
  ENDMETHOD.

  METHOD get_valid_total_distance.
    cl_abap_unit_assert=>assert_equals( exp = 11 act = cut->calculate_total_distance( ) ).
  ENDMETHOD.

  METHOD get_similarity_score.
    cl_abap_unit_assert=>assert_equals( exp = 31 act = cut->calculate_similarity_score( ) ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(location_list_builder) = NEW simple_list_comparison( input_data ).

  WRITE /: |The result of part 1 is: { location_list_builder->calculate_total_distance( ) }|.
  WRITE /: |The result of part 2 is: { location_list_builder->calculate_similarity_score( ) }|.
