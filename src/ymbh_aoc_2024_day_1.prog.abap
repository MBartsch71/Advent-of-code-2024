REPORT ymbh_aoc_2024_day_1.

CLASS simple_list_comparison DEFINITION FINAL.

  PUBLIC SECTION.
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

    METHODS constructor IMPORTING input_data TYPE stringtab.

    METHODS split_list.

    METHODS combine_lists.

    METHODS get_combined_list RETURNING VALUE(result) TYPE location_list.

    METHODS get_total_distance RETURNING VALUE(result) TYPE i.
    METHODS build_similarities.
    METHODS get_similarity_list RETURNING VALUE(result) TYPE similarity_list.
    METHODS get_similarity_score
      RETURNING
        VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA input_data TYPE stringtab.
    DATA left_list TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
    DATA right_list TYPE SORTED TABLE OF string WITH NON-UNIQUE KEY table_line.
    DATA combined_list TYPE location_list.
    DATA similarities TYPE similarity_list.

    METHODS check_distance IMPORTING left_item     TYPE i
                                     right_item    TYPE i
                           RETURNING VALUE(result) TYPE i.

ENDCLASS.

CLASS simple_list_comparison IMPLEMENTATION.

  METHOD constructor.
    me->input_data = input_data.
  ENDMETHOD.

  METHOD split_list.
    LOOP AT input_data REFERENCE INTO DATA(line).
      SPLIT line->* AT space INTO TABLE DATA(values).
      left_list = VALUE #( BASE left_List ( values[ 1 ] ) ).
      right_list = VALUE #( BASE right_list ( values[ lines( values ) ] ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD combine_lists.
    DATA(list_length) = lines( left_list ).
    DATA(loop_index) = 1.

    DO list_length TIMES.
      combined_list = VALUE #( BASE combined_list ( left_list_value = left_list[ loop_index ]
                                                    right_list_value = right_list[ loop_index ]
                                                    distance = check_distance( left_item = CONV #( left_list[ loop_index ] )
                                                                               right_item = CONV #( right_list[ loop_index ] ) ) ) ).
      loop_index += 1.
    ENDDO.
  ENDMETHOD.

  METHOD get_combined_list.
    result = combined_list.
  ENDMETHOD.

  METHOD check_distance.
    result = abs( left_item - right_item ).
  ENDMETHOD.

  METHOD get_total_distance.
    result = REDUCE #( INIT sum = 0
                       FOR line IN combined_list
                       NEXT sum = sum + line-distance ).
  ENDMETHOD.

  METHOD build_similarities.
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


  METHOD get_similarity_list.
    result = similarities.
  ENDMETHOD.

  METHOD get_similarity_score.
    result = REDUCE #( INIT score = 0
                       FOR line IN similarities
                       NEXT score = score + line-appears_in_right_list ).
  ENDMETHOD.

ENDCLASS.

CLASS test_list_comparison DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
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

    DATA  cut TYPE REF TO simple_list_comparison.

    METHODS setup.
    METHODS get_valid_total_distance FOR TESTING.
    METHODS get_similarity_list FOR TESTING.
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
    cut->split_list( ).
    cut->combine_lists( ).
    cl_abap_unit_assert=>assert_equals( exp = 11 act = cut->get_total_distance( ) ).
  ENDMETHOD.

  METHOD get_similarity_list.
    DATA(expected_data) = VALUE similarity_list( ( left_list_value = 1 appears_in_right_list = 0 )
                                                 ( left_list_value = 2 appears_in_right_list = 0 )
                                                 ( left_list_value = 3 appears_in_right_list = 9 )
                                                 ( left_list_value = 3 appears_in_right_list = 9 )
                                                 ( left_list_value = 3 appears_in_right_list = 9 )
                                                 ( left_list_value = 4 appears_in_right_list = 4 ) ).
    cut->split_list( ).
    cut->build_similarities( ).
    cl_abap_unit_assert=>assert_equals( exp = expected_data act = cut->get_similarity_list( ) ).
  ENDMETHOD.

  METHOD get_similarity_score.
    cut->split_list( ).
    cut->build_similarities( ).
    cl_abap_unit_assert=>assert_equals( exp = 31 act = cut->get_similarity_score( ) ).
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(location_list_builder) = NEW simple_list_comparison( input_data ).

  location_list_builder->split_list( ).
  location_list_builder->combine_lists( ).
  WRITE /: |The result of part 1 is: { location_list_builder->get_total_distance( ) }|.

  location_list_builder->build_similarities( ).
  WRITE /: |The result of part 2 is: { location_list_builder->get_similarity_score( ) }|.
