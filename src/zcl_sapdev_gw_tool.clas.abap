"! <p class="shorttext synchronized" lang="en">GateWay Tools</p>
"! <p>Author: <strong>Attila Berencsi, sapdev.eu</strong><p>
"! <p>Version Info (YYMMDD): <strong>v220123</strong><p>
"! <p>https://github.com/attilaberencsi/gwtools<p>
"! <p>Licence: MIT<p>
CLASS zcl_sapdev_gw_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_srv_id_range,
        sign   TYPE c LENGTH 1,
        option TYPE c LENGTH 2,
        low    TYPE /iwfnd/med_mdl_srg_identifier,
        high   TYPE /iwfnd/med_mdl_srg_identifier,
      END OF ty_srv_id_range.

    TYPES ty_srv_id_ranges TYPE STANDARD TABLE OF ty_srv_id_range.

    TYPES:
      ty_output_mode TYPE i.

    CONSTANTS:
      BEGIN OF gc_output_mode,
        no_output  TYPE ty_output_mode VALUE 0, "Not yet supported
        gui_output TYPE ty_output_mode VALUE 1,
        string_tab TYPE ty_output_mode VALUE 2,
      END OF gc_output_mode.

    DATA:
      output_mode TYPE ty_output_mode READ-ONLY.

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Setup</p>
      "!
      "! @parameter i_output_mode | <p class="shorttext synchronized" lang="en">Output mode (GUI/string_table)</p>
      constructor
        IMPORTING
          i_output_mode TYPE ty_output_mode DEFAULT zcl_sapdev_gw_tool=>gc_output_mode-gui_output,

      wipe_client_cache
        IMPORTING
          i_just_for_username TYPE syuname OPTIONAL
        RETURNING
          VALUE(r_output)     TYPE list_string_table,

      wipe_global_cache
        RETURNING
          VALUE(r_output) TYPE list_string_table,

      wipe_odata_meta_cache
        IMPORTING
          i_service_ranges TYPE zcl_sapdev_gw_tool=>ty_srv_id_ranges
        RETURNING
          VALUE(r_output)  TYPE list_string_table,

      "! <p class="shorttext synchronized" lang="en">Invalidate all $metadata+annotation cache tokens-all clients</p>
      "!
      "! @parameter r_output | <p class="shorttext synchronized" lang="en">Log</p>
      wipe_odata_meta_cache_token
        RETURNING
          VALUE(r_output) TYPE list_string_table,

      calc_app_index
        IMPORTING
          i_repo          TYPE /ui5/ui5_repository_ui OPTIONAL
        RETURNING
          VALUE(r_output) TYPE list_string_table,

      get_show_icf_active
        IMPORTING
          i_show_ui5_odata_only TYPE abap_bool OPTIONAL
        EXPORTING
          e_services            TYPE icf_exchg_pub_ttyp
          e_output              TYPE list_string_table,

      get_show_icf_inactive
        IMPORTING
          i_show_ui5_odata_only TYPE abap_bool OPTIONAL
        EXPORTING
          e_services            TYPE icf_exchg_pub_ttyp
          e_output              TYPE list_string_table.


  PROTECTED SECTION.
    METHODS:
      build_icfservice_fcat RETURNING VALUE(r_result) TYPE slis_t_fieldcat_alv,

      retrieve_list_output
        IMPORTING
          i_free          TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(r_output) TYPE list_string_table,

      itab_to_csv
        IMPORTING
          i_tab        TYPE INDEX TABLE
          i_separator  TYPE clike DEFAULT ';'
        RETURNING
          VALUE(r_csv) TYPE list_string_table.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_sapdev_gw_tool IMPLEMENTATION.

  METHOD constructor.
    me->output_mode = i_output_mode.
  ENDMETHOD.

  METHOD wipe_client_cache.
    "Do we have this ?
    SELECT SINGLE @abap_true FROM tadir INTO @DATA(exists)
      WHERE pgmid = 'R3TR'
    AND object = 'PROG'
    AND obj_name = '/UI2/INVALIDATE_CLIENT_CACHES'.

    IF sy-subrc NE 0.
      IF me->output_mode = gc_output_mode-gui_output.
        MESSAGE 'Report /UI2/INVALIDATE_CLIENT_CACHES does not exist' TYPE 'I' DISPLAY LIKE 'E'. "#EC NOTEXT
      ELSE.
        APPEND |/UI2/INVALIDATE_CLIENT_CACHES| TO r_output.
      ENDIF.
      RETURN.
    ENDIF.

    IF i_just_for_username IS INITIAL.
      IF me->output_mode = gc_output_mode-gui_output.
        SUBMIT /ui2/invalidate_client_caches WITH gv_all = abap_true AND RETURN. "#EC CI_SUBMIT
      ELSE.
        SUBMIT /ui2/invalidate_client_caches WITH gv_all = abap_true EXPORTING LIST TO MEMORY AND RETURN. "#EC CI_SUBMIT
        r_output = retrieve_list_output( ).
      ENDIF.
    ELSE.
      IF me->output_mode = gc_output_mode-gui_output.
        SUBMIT /ui2/invalidate_client_caches
          WITH gv_all = abap_false
          WITH gv_user = abap_true
          WITH g_uname = i_just_for_username AND RETURN. "#EC CI_SUBMIT
      ELSE.
        SUBMIT /ui2/invalidate_client_caches
          WITH gv_all = abap_false
          WITH gv_user = abap_true
          WITH g_uname = i_just_for_username EXPORTING LIST TO MEMORY AND RETURN. "#EC CI_SUBMIT

        r_output = retrieve_list_output( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD wipe_global_cache.

    "Do we have this ?
    SELECT SINGLE @abap_true FROM tadir INTO @DATA(exists)
      WHERE pgmid = 'R3TR'
    AND object = 'PROG'
    AND obj_name = '/UI2/INVALIDATE_GLOBAL_CACHES'.

    IF sy-subrc NE 0.
      IF me->output_mode = gc_output_mode-gui_output.
        MESSAGE 'Report /UI2/INVALIDATE_GLOBAL_CACHES does not exist' TYPE 'I' DISPLAY LIKE 'E'. "#EC NOTEXT
      ELSE.
        APPEND |Report /UI2/INVALIDATE_GLOBAL_CACHES does not exist| TO r_output. "#EC NOTEXT
      ENDIF.
      RETURN.
    ENDIF.

    IF me->output_mode = gc_output_mode-gui_output.
      SUBMIT /ui2/invalidate_global_caches               "#EC CI_SUBMIT
        WITH gv_test = abap_false
        WITH gv_exe = abap_true AND RETURN.
    ELSE.
      SUBMIT /ui2/invalidate_global_caches               "#EC CI_SUBMIT
        WITH gv_test = abap_false
        WITH gv_exe = abap_true EXPORTING LIST TO MEMORY AND RETURN.

      r_output = retrieve_list_output( ).
    ENDIF.

  ENDMETHOD.

  METHOD wipe_odata_meta_cache.

    IF lines(  i_service_ranges ) = 0.
      IF me->output_mode = gc_output_mode-gui_output.
        MESSAGE 'Please select at least one service' TYPE 'I' DISPLAY LIKE 'E'. "#EC NOTEXT
      ELSE.
        APPEND |Please select at least one service| TO r_output. "#EC NOTEXT
      ENDIF.
      RETURN.
    ENDIF.

    SELECT * FROM /iwfnd/i_med_srh INTO TABLE @DATA(services)
      WHERE srv_identifier IN @i_service_ranges.

    LOOP AT services INTO DATA(service).

      /iwfnd/cl_sutil_moni=>cleanup_metadata_cache(
        EXPORTING
          iv_mode            = 'A'
          iv_multi_origin    = abap_true
          iv_namespace       = service-namespace  "'/SAP/'
          iv_service_name    = service-service_name
          iv_service_version = service-service_version
        IMPORTING
          ev_error_text      = DATA(error_text)
      ).

      IF error_text IS NOT INITIAL.
        IF me->output_mode = gc_output_mode-gui_output.
          WRITE: / icon_error_protocol AS ICON, service-srv_identifier.
          WRITE: / '  ', error_text.
        ELSE.
          APPEND |ERROR: { service-srv_identifier } | TO r_output.
          APPEND |  { service-srv_identifier }| TO r_output.
        ENDIF.
        CONTINUE.
      ELSE.
        IF me->output_mode = gc_output_mode-gui_output.
          WRITE: / icon_okay AS ICON, service-srv_identifier.
        ELSE.
          APPEND |Wiped: { service-srv_identifier }| TO r_output. "#EC NOTEXT
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF sy-subrc <> 0.
      DATA(no_hits) = '! NO SERVICES FOUND FOR YOUR SELECTION !'.
      IF me->output_mode = gc_output_mode-gui_output.
        WRITE no_hits.
      ELSE.
        APPEND |{ no_hits }| TO r_output.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_show_icf_active.

    cl_icf_service_publication=>get_activate_nodes( IMPORTING it_icf_exchg_pub = DATA(active_services) ).

    IF me->output_mode = gc_output_mode-gui_output.

      "Setup and Display List
      IF i_show_ui5_odata_only = abap_true."Filter on UI5 and OData services by default
        DATA(list_filter) = VALUE slis_t_filter_alv(
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/ui5_ui5/*')
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/opu/*')
        ).
      ENDIF.

      DATA(field_catalog) = build_icfservice_fcat( ).

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_structure_name = 'ICF_EXCHG_PUB'
          i_grid_title     = CONV lvc_title( 'Active Services' )  "#EC NOTEXT
          is_layout        = VALUE slis_layout_alv( zebra = abap_true colwidth_optimize = abap_true cell_merge = 'N' )
          it_filter        = list_filter
          it_fieldcat      = field_catalog
        TABLES
          t_outtab         = active_services
        EXCEPTIONS
          program_error    = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.
      IF i_show_ui5_odata_only = abap_true."Filter on UI5 and OData services by default
        LOOP AT active_services TRANSPORTING NO FIELDS WHERE ( path NP '/sap/bc/ui5_ui5/*' AND path NP '/sap/opu/*' ).
          DELETE active_services.
        ENDLOOP.
      ENDIF.

      IF e_services IS REQUESTED.
        e_services = active_services.
      ENDIF.

      IF e_output IS REQUESTED.
        e_output = itab_to_csv( i_tab = active_services ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_show_icf_inactive.
    cl_icf_service_publication=>get_inactive_nodes( IMPORTING et_icf_exchg_pub  = DATA(inactive_services) ).

    IF me->output_mode = gc_output_mode-gui_output.

      "Setup and Display List
      IF i_show_ui5_odata_only = abap_true."Filter on UI5 and OData services by default
        DATA(list_filter) = VALUE slis_t_filter_alv(
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/ui5_ui5/*')
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/opu/*')
        ).
      ENDIF.

      DATA(field_catalog) = build_icfservice_fcat( ).

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_structure_name = 'ICF_EXCHG_PUB'
          i_grid_title     = CONV lvc_title( 'Inactive Services' )  "#EC NOTEXT
          is_layout        = VALUE slis_layout_alv( zebra = abap_true colwidth_optimize = abap_true cell_merge = 'N' )
          it_filter        = list_filter
          it_fieldcat      = field_catalog
        TABLES
          t_outtab         = inactive_services
        EXCEPTIONS
          program_error    = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ELSE.
      IF i_show_ui5_odata_only = abap_true."Filter on UI5 and OData services by default
        LOOP AT inactive_services TRANSPORTING NO FIELDS WHERE ( path NP '/sap/bc/ui5_ui5/*' AND path NP '/sap/opu/*' ).
          DELETE inactive_services.
        ENDLOOP.
      ENDIF.

      IF e_services IS REQUESTED.
        e_services = inactive_services.
      ENDIF.

      IF e_output IS REQUESTED.
        e_output = itab_to_csv( i_tab = inactive_services ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD build_icfservice_fcat.

    CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
      EXPORTING
        i_program_name         = sy-cprog
        i_structure_name       = 'ICF_EXCHG_PUB'
      CHANGING
        ct_fieldcat            = r_result
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT r_result ASSIGNING FIELD-SYMBOL(<field_meta>).
      CASE <field_meta>-fieldname.
        WHEN 'VHOST' OR 'REF_VHOST' OR 'CREATION_DATE' OR 'CREATION_TIME' OR 'COUNTER'.
          <field_meta>-no_out =  abap_true.
        WHEN 'PATH'.
          <field_meta>-seltext_s = 'Serv/Alias'.
          <field_meta>-seltext_m = <field_meta>-seltext_l = <field_meta>-reptext_ddic = 'Service/Alias'. "#EC NOTEXT
        WHEN 'REF_PATH'.
          <field_meta>-seltext_s = 'AliasedSrv' .
          <field_meta>-seltext_m = <field_meta>-seltext_l = <field_meta>-reptext_ddic = 'Aliased Service'. "#EC NOTEXT
        WHEN 'PUBLIC_SERVICE'.
          <field_meta>-seltext_s = <field_meta>-seltext_m = <field_meta>-seltext_l = <field_meta>-reptext_ddic = 'Public'. "#EC NOTEXT
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.

  METHOD retrieve_list_output.

    DATA:
      listobject  TYPE STANDARD TABLE OF abaplist.

    CALL FUNCTION 'LIST_FROM_MEMORY'
      TABLES
        listobject = listobject
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'LIST_TO_ASCI'
      IMPORTING
        list_string_ascii  = r_output
      TABLES
        listobject         = listobject
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      "Oops!... I Did It Again
    ENDIF.

    IF i_free = abap_true.
      CALL FUNCTION 'LIST_FREE_MEMORY'.
    ENDIF.

  ENDMETHOD.

  METHOD itab_to_csv.
    DATA:
      csv_line TYPE string.

    FIELD-SYMBOLS:
      <value> TYPE any.

    LOOP AT i_tab ASSIGNING FIELD-SYMBOL(<structure>).
      CLEAR csv_line.

      DO.
        ASSIGN COMPONENT sy-index OF STRUCTURE <structure> TO <value>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        IF sy-index = 1.
          csv_line = |{ <value> ALPHA = OUT } |.
        ELSE.
          csv_line = |{ csv_line }{ i_separator }{ <value> ALPHA = OUT } |.
        ENDIF.
      ENDDO.

      APPEND csv_line TO r_csv.
    ENDLOOP.

  ENDMETHOD.

  METHOD calc_app_index.

    "Do we have this ?
    SELECT SINGLE @abap_true FROM tadir INTO @DATA(exists)
      WHERE pgmid = 'R3TR'
    AND object = 'PROG'
    AND obj_name = '/UI5/APP_INDEX_CALCULATE'.

    IF sy-subrc NE 0.
      IF me->output_mode = gc_output_mode-gui_output.
        MESSAGE 'Report /UI5/APP_INDEX_CALCULATE does not exist' TYPE 'I' DISPLAY LIKE 'E'. "#EC NOTEXT
      ELSE.
        APPEND |Report /UI5/APP_INDEX_CALCULATE does not exist| TO r_output. "#EC NOTEXT
      ENDIF.
      RETURN.
    ENDIF.

    IF me->output_mode = gc_output_mode-gui_output.
      IF i_repo IS INITIAL.
        SUBMIT /ui5/app_index_calculate                  "#EC CI_SUBMIT
          WITH p_all_a = abap_true
          WITH p_all_d = abap_false AND RETURN.
      ELSE.
        SUBMIT /ui5/app_index_calculate                  "#EC CI_SUBMIT
          WITH p_all = abap_false
          WITH p_distl = abap_false
          WITH p_repo = i_repo AND RETURN.
      ENDIF.
    ELSE.
      IF i_repo IS INITIAL.
        SUBMIT /ui5/app_index_calculate                  "#EC CI_SUBMIT
          WITH p_all_a = abap_true
          WITH p_all_d = abap_false EXPORTING LIST TO MEMORY AND RETURN.
      ELSE.
        SUBMIT /ui5/app_index_calculate                  "#EC CI_SUBMIT
          WITH p_all = abap_false
          WITH p_distl = abap_false
          WITH p_repo = i_repo EXPORTING LIST TO MEMORY AND RETURN.
      ENDIF.

      r_output = retrieve_list_output( ).
    ENDIF.

  ENDMETHOD.

  METHOD wipe_odata_meta_cache_token.

    "This method is the adjusted copy of report /ui5/del_odata_metadata_cache
    "without the final message statement, which would dump in HTTP Plugin Mode.
    "Omitting the naming conventions is on purpose, because tracking the changes of
    "the standard is more easier.

    TRY.
        IF sy-batch <> abap_true.
          AUTHORITY-CHECK OBJECT 'S_SUISUPRT'
                       ID 'ACTVT' FIELD '02'
                       ID 'SUI_AREA' FIELD 'UI5'.
          IF sy-subrc <> 0.
            AUTHORITY-CHECK OBJECT 'S_DEVELOP'
                      ID 'DEVCLASS' DUMMY
                      ID 'OBJTYPE' DUMMY
                      ID 'OBJNAME' DUMMY
                      ID 'P_GROUP' DUMMY
                      ID 'ACTVT' FIELD '02'.
            IF sy-subrc <> 0.
              MESSAGE i001(/ui5/check_appidx).
*   Missing authority to change application index or metadata cache
              RETURN.
            ENDIF.
          ENDIF.
        ENDIF.

        DATA: lo_app_index TYPE REF TO /ui5/cl_ui5_app_index.

        lo_app_index ?= /ui5/cl_ui5_app_api_factory=>get_app_index_instance( ).

        lo_app_index->invalidate_backend_contexts( ).

* SAPDEV: Custom Code - BEGIN
        IF me->output_mode = gc_output_mode-gui_output.
          MESSAGE 'Backend context tokens have been invalidated successfully' TYPE 'S'. "#EC NOTEXT
        ELSE.
          APPEND |Backend context tokens have been invalidated successfully| TO r_output. "#EC NOTEXT
        ENDIF.

      CATCH cx_root INTO DATA(ex_root).
        IF me->output_mode = gc_output_mode-gui_output.
          MESSAGE ex_root->get_text( ) TYPE 'I' DISPLAY LIKE 'E'. "#EC NOTEXT
        ELSE.
          APPEND ex_root->get_text( ) TO r_output.          "#EC NOTEXT
        ENDIF.
    ENDTRY.
* SAPDEV: Custom Code - END

  ENDMETHOD.

ENDCLASS.
