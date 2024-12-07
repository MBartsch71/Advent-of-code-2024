REPORT ymbh_aoc_2024_day_6.

INTERFACE local_types.
  TYPES: BEGIN OF guard_pos,
           y TYPE i,
           x TYPE i,
         END OF guard_pos.
  TYPES: BEGIN OF direction_item,
           current_heading TYPE char1,
           new_heading     TYPE char1,
         END OF direction_item.
  TYPES directions TYPE STANDARD TABLE OF direction_item WITH EMPTY KEY.

  TYPES: BEGIN OF map_item,
           x     TYPE i,
           y     TYPE i,
           value TYPE c LENGTH 1,
         END OF map_item.
  TYPES map_items TYPE SORTED TABLE OF map_item WITH UNIQUE KEY primary_key COMPONENTS y x.
ENDINTERFACE.

CLASS guard DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor IMPORTING position TYPE local_types=>guard_pos.
    METHODS place IMPORTING position TYPE local_types=>guard_pos.
    METHODS position RETURNING VALUE(result) TYPE local_types=>guard_pos.
    METHODS turn.
    METHODS step.
    METHODS steps_made RETURNING VALUE(result) TYPE i.
    METHODS look_ahead RETURNING VALUE(result) TYPE local_types=>guard_pos.

  PRIVATE SECTION.
    DATA current_position TYPE local_types=>guard_pos.
    DATA current_heading TYPE char1.
    DATA guard_directions TYPE local_types=>directions.
    DATA steps TYPE i.
ENDCLASS.

CLASS guard IMPLEMENTATION.

  METHOD constructor.
    current_heading = |N|.
    current_position = position.
    guard_directions = VALUE local_types=>directions( ( current_heading = |N| new_heading = |E| )
                                                      ( current_heading = |E| new_heading = |S| )
                                                      ( current_heading = |S| new_heading = |W| )
                                                      ( current_heading = |W| new_heading = |N| ) ).
  ENDMETHOD.

  METHOD place.
    me->current_position = position.
  ENDMETHOD.

  METHOD position.
    result = current_position.
  ENDMETHOD.

  METHOD turn.
    current_heading = guard_directions[ current_heading = current_heading ]-new_heading.
  ENDMETHOD.

  METHOD step.
    DATA(current_y) = current_position-y.
    DATA(current_x) = current_position-x.

    current_y = SWITCH #( current_heading WHEN 'N' THEN current_y - 1
                                          WHEN 'S' THEN current_y + 1
                                          ELSE current_y ).

    current_x = SWITCH #( current_heading WHEN 'E' THEN current_x + 1
                                          WHEN 'W' THEN current_x - 1
                                          ELSE current_x ).

    current_position = VALUE #( y = current_y x = current_x ).
    steps += 1.
  ENDMETHOD.

  METHOD steps_made.
    result = steps.
  ENDMETHOD.

  METHOD look_ahead.
    DATA(next_y) = SWITCH i( current_heading WHEN 'N' THEN current_position-y - 1
                                             WHEN 'S' THEN current_position-y + 1
                                             ELSE current_position-y ).
    DATA(next_x) = SWITCH i( current_heading WHEN 'E' THEN current_position-x + 1
                                             WHEN 'W' THEN current_position-x - 1
                                             ELSE current_position-x ).
    result = VALUE #( y = next_y x = next_x ).
  ENDMETHOD.

ENDCLASS.

CLASS map DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS constructor IMPORTING input TYPE stringtab.

    METHODS get_initial_map RETURNING VALUE(result) TYPE local_types=>map_items.
    METHODS find_guard_at_map RETURNING VALUE(result) TYPE local_types=>map_item.
    METHODS get_field IMPORTING field_ahead   TYPE local_types=>guard_pos
                      RETURNING VALUE(result) TYPE local_types=>map_item.
    METHODS mark_position_visited
      IMPORTING
        guard_position TYPE local_types=>guard_pos.
    METHODS get_visited_locations_amount
      RETURNING
        VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA input TYPE stringtab.
    DATA map TYPE local_types=>map_items.
    METHODS build_map.

ENDCLASS.

CLASS map IMPLEMENTATION.

  METHOD constructor.
    me->input = input.
    build_map( ).
  ENDMETHOD.

  METHOD get_initial_map.
    result = map.
  ENDMETHOD.

  METHOD build_map.
    DATA(table_width) = strlen( input[ 1 ] ).

    LOOP AT input REFERENCE INTO DATA(line).
      DATA(y) = sy-tabix.
      DATA(offset) = 0.

      DO table_width TIMES.
        DATA(x) = sy-index.
        map = VALUE #( BASE map
                        ( y     = y
                          x     = x
                          value = substring( val = line->* off = offset len = 1 ) ) ).
        offset += 1.
      ENDDO.
    ENDLOOP.
  ENDMETHOD.

  METHOD find_guard_at_map.
    FIND FIRST OCCURRENCE OF '^' IN TABLE input RESULTS DATA(matches).
    result = VALUE #( y = matches-line x = matches-offset + 1 value = |^| ).
  ENDMETHOD.

  METHOD get_field.
    TRY.
        result = map[ y = field_ahead-y x = field_ahead-x ].
      CATCH cx_sy_itab_line_not_found.
        result = VALUE #( x = -1 y = -1 value = '?').
    ENDTRY.
  ENDMETHOD.

  METHOD mark_position_visited.
    map[ y = guard_position-y x = guard_position-x ]-value = 'X'.
  ENDMETHOD.

  METHOD get_visited_locations_amount.
    result = REDUCE #( INIT sum = 0
                       FOR line IN map
                       NEXT sum = SWITCH #( line-value WHEN 'X'
                                                THEN sum + 1
                                                ELSE sum ) ).
  ENDMETHOD.

ENDCLASS.

CLASS navigation DEFINITION FINAL.

  PUBLIC SECTION.

    METHODS set_map IMPORTING map TYPE REF TO map.
    METHODS get_map RETURNING VALUE(result) TYPE REF TO map.
    METHODS setup_guard.
    METHODS get_guard_position RETURNING VALUE(result) TYPE local_types=>guard_pos.
    METHODS get_field_ahead_of_guard RETURNING VALUE(result) TYPE char1.
    METHODS send_guard.
    METHODS guard_reached_obstacle RETURNING VALUE(result) TYPE abap_bool.
    METHODS get_guard RETURNING VALUE(result) TYPE REF TO guard.
    METHODS send_guard_to_patrol RETURNING VALUE(result) TYPE REF TO guard.
    METHODS get_distinct_visited_locations RETURNING VALUE(result) TYPE i.

  PRIVATE SECTION.
    DATA map TYPE REF TO map.
    DATA guard TYPE REF TO guard.
    METHODS obstacle_ahead RETURNING VALUE(result) TYPE abap_bool.
    METHODS end_of_patrol RETURNING VALUE(result) TYPE abap_bool.

ENDCLASS.

CLASS navigation IMPLEMENTATION.

  METHOD set_map.
    me->map = map.
  ENDMETHOD.

  METHOD get_map.
    result = map.
  ENDMETHOD.

  METHOD setup_guard.
    DATA(guard_pos) = map->find_guard_at_map( ).
    guard = NEW guard( VALUE #( y = guard_pos-y x = guard_pos-x ) ).
    map->mark_position_visited( VALUE #( y = guard_pos-y x = guard_pos-x ) ).
  ENDMETHOD.

  METHOD get_guard_position.
    result = guard->position( ).
  ENDMETHOD.

  METHOD get_field_ahead_of_guard.
    DATA(field_ahead) = guard->look_ahead( ).
    DATA(check_field_at_map) = map->get_field( field_ahead ).
    result = check_field_at_map-value.
  ENDMETHOD.

  METHOD send_guard.
    guard->step( ).
  ENDMETHOD.

  METHOD guard_reached_obstacle.
    WHILE NOT obstacle_ahead( ).
      guard->step( ).
    ENDWHILE.

    result = abap_true.
  ENDMETHOD.

  METHOD get_guard.
    result = guard.
  ENDMETHOD.

  METHOD obstacle_ahead.
    result = xsdbool( get_field_ahead_of_guard( ) = '#' ).
  ENDMETHOD.

  METHOD send_guard_to_patrol.
    DO.
      IF end_of_patrol( ).
        result = guard.
        RETURN.
      ELSEIF obstacle_ahead( ).
        guard->turn( ).
      ELSE.
        guard->step( ).
        map->mark_position_visited( guard->position( ) ).
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD end_of_patrol.
    result = xsdbool( get_field_ahead_of_guard( ) = '?' ).
  ENDMETHOD.

  METHOD get_distinct_visited_locations.
    result = map->get_visited_locations_amount( ).
  ENDMETHOD.

ENDCLASS.

CLASS test_map DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO map.
    METHODS setup.

    METHODS get_input_as_initial_map FOR TESTING.
    METHODS find_guard FOR TESTING.

ENDCLASS.

CLASS test_map IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( VALUE stringtab( ( |....#.....| )
                                  ( |.........#| )
                                  ( |..........| )
                                  ( |..#.......| )
                                  ( |.......#..| )
                                  ( |..........| )
                                  ( |.#..^.....| )
                                  ( |........#.| )
                                  ( |#.........| )
                                  ( |......#...| ) ) ).
  ENDMETHOD.

  METHOD get_input_as_initial_map.
    DATA(expected_Values) = VALUE local_types=>map_items( FOR x = 1 THEN x + 1 UNTIL x > 10
                                                          FOR y = 1 THEN y + 1 UNTIL y > 10
                                                           ( x = x y = y value = |.| ) ).
    expected_Values[ y = 1  x = 5 ]-value  = |#|.
    expected_Values[ y = 2  x = 10 ]-value = |#|.
    expected_Values[ y = 4  x = 3 ]-value  = |#|.
    expected_Values[ y = 5  x = 8 ]-value  = |#|.
    expected_Values[ y = 7  x = 2 ]-value  = |#|.
    expected_Values[ y = 7  x = 5 ]-value  = |^|.
    expected_Values[ y = 8  x = 9 ]-value  = |#|.
    expected_Values[ y = 9  x = 1 ]-value  = |#|.
    expected_Values[ y = 10 x = 7 ]-value  = |#|.
    cl_abap_unit_assert=>assert_equals( exp = expected_values act = cut->get_initial_map( ) ).
  ENDMETHOD.

  METHOD find_guard.
    DATA(expected_position) = VALUE local_types=>map_item( y = 7 x = 5 value = |^| ).
    cl_abap_unit_assert=>assert_equals( exp = expected_position act = cut->find_guard_at_map( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS test_guard DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO guard.

    METHODS setup.

    METHODS get_guard_initial_position FOR TESTING.

    METHODS get_pos_after_step FOR TESTING.
    METHODS get_pos_after_step_and_turn FOR TESTING.
    METHODS get_amount_of_steps FOR TESTING.
    METHODS get_pos_of_next_step FOR TESTING.
ENDCLASS.

CLASS test_guard IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( VALUE #( y = 2 x = 3 ) ).
  ENDMETHOD.

  METHOD get_guard_initial_position.
    DATA(expected_value) = VALUE local_types=>guard_pos( y = 2 x = 3 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_value  act = cut->position( )  ).
  ENDMETHOD.

  METHOD get_pos_after_step.
    cut->step( ).
    DATA(expected_Values) = VALUE local_types=>guard_pos( y = 1 x = 3 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_Values act = cut->position( )  ).
  ENDMETHOD.

  METHOD get_pos_after_step_and_turn.
    cut->step( ).
    cut->turn( ).
    cut->Step( ).
    DATA(expected_Value) = VALUE local_types=>guard_pos( y = 1 x = 4 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_value act = cut->position( ) ).
  ENDMETHOD.

  METHOD get_amount_of_steps.
    cut->step( ).
    cut->step( ).
    cut->turn( ).
    cut->step( ).
    cut->step( ).
    cl_abap_unit_assert=>assert_equals( exp = 4 act = cut->steps_made( ) ).
  ENDMETHOD.

  METHOD get_pos_of_next_step.
    DATA(expected_position) = VALUE local_types=>guard_pos( y = 1 x = 3 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_position act = cut->look_ahead( ) ).
  ENDMETHOD.

ENDCLASS.

CLASS test_navigation DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    DATA cut TYPE REF TO navigation.

    METHODS setup.
    METHODS can_create_object FOR TESTING.
    METHODS setup_map_is_correct FOR TESTING.
    METHODS check_guard_is_found FOR TESTING.
    METHODS get_field_ahead_of_guard FOR TESTING.
    METHODS get_first_obstacle FOR TESTING.
    METHODS send_guard_till_obstacle FOR TESTING.
    METHODS guard_patrols_till_oob FOR TESTING.
ENDCLASS.

CLASS test_navigation IMPLEMENTATION.

  METHOD setup.
    cut = NEW #( ).
    DATA(input) = VALUE stringtab( ( |....#.....| )
                                   ( |.........#| )
                                   ( |..........| )
                                   ( |..#.......| )
                                   ( |.......#..| )
                                   ( |..........| )
                                   ( |.#..^.....| )
                                   ( |........#.| )
                                   ( |#.........| )
                                   ( |......#...| ) ).
    DATA(map) = NEW map( input ).
    cut->set_map( map ).
    cut->setup_guard( ).
  ENDMETHOD.

  METHOD can_create_object.
    cl_abap_unit_assert=>assert_bound( act = cut msg = |The object should be bound!| ).
  ENDMETHOD.

  METHOD setup_map_is_correct.
    cl_abap_unit_assert=>assert_equals( exp = 100 act = lines( cut->get_map( )->get_initial_map( ) )  ).
  ENDMETHOD.

  METHOD check_guard_is_found.
    DATA(expected_guard_position) = VALUE local_types=>guard_pos( y = 7 x = 5 ).
    cl_abap_unit_assert=>assert_equals( exp = expected_guard_position act = cut->get_guard_position( ) ).
  ENDMETHOD.

  METHOD get_field_ahead_of_guard.
    cl_abap_unit_assert=>assert_equals( exp = |.| act = cut->get_field_ahead_of_guard( )  ).
  ENDMETHOD.

  METHOD get_first_obstacle.
    DO 5 TIMES.
      cut->send_guard( ).
    ENDDO.
    cl_abap_unit_assert=>assert_equals( exp = |#| act = cut->get_field_ahead_of_guard( ) ).
  ENDMETHOD.

  METHOD send_guard_till_obstacle.
    cl_abap_unit_assert=>assert_true( act = cut->guard_reached_obstacle( ) ).
    DATA(guard) = cut->get_guard( ).
    cl_abap_unit_assert=>assert_equals( exp = 5 act = guard->steps_made( )  ).
  ENDMETHOD.

  METHOD guard_patrols_till_oob.
    DATA(guard_finished_patrol) = cut->send_guard_to_patrol( ).
    cl_abap_unit_assert=>assert_equals( exp = 41  act = cut->get_distinct_visited_locations( ) ).
  ENDMETHOD.

ENDCLASS.


START-OF-SELECTION.
  DATA(input_data) = NEW zcl_mbh_file_upload( )->file_upload_in_stringtab( ).
  DATA(navigation) = NEW navigation( ).
  navigation->set_map( NEW map( input_data ) ).
  navigation->setup_guard( ).
  navigation->send_guard_to_patrol( ).

  WRITE /: |The result of part 1 is: { navigation->get_distinct_visited_locations( ) }|.
  WRITE /: |The result of part 2 is: tbd|.
