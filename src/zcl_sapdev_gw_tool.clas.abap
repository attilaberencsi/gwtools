"! <p class="shorttext synchronized" lang="en">GateWay Tools</p>
"! <p>Version Info (YYMMDD): <strong>v220611</strong><p>
"! <p>https://github.com/attilaberencsi/gwtools<p>
"! <p>Licence: MIT<p>
CLASS zcl_sapdev_gw_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_sapdev_gw_tool.
    ALIASES: wipe_client_cache FOR zif_sapdev_gw_tool~wipe_client_cache.
    ALIASES: wipe_global_cache FOR zif_sapdev_gw_tool~wipe_global_cache.
    ALIASES: wipe_odata_meta_cache FOR zif_sapdev_gw_tool~wipe_odata_meta_cache.
    ALIASES: wipe_odata_meta_cache_token FOR zif_sapdev_gw_tool~wipe_odata_meta_cache_token.
    ALIASES: calc_app_index FOR zif_sapdev_gw_tool~calc_app_index.
    ALIASES: get_show_icf_active FOR zif_sapdev_gw_tool~get_show_icf_active.
    ALIASES: get_show_icf_inactive FOR zif_sapdev_gw_tool~get_show_icf_inactive.

    ALIASES gc_output_mode FOR zif_sapdev_gw_tool~gc_output_mode.

    DATA:
      output_mode TYPE zif_sapdev_gw_tool=>ty_output_mode READ-ONLY.

    "! <p class="shorttext synchronized" lang="en">Setup</p>
    "!
    "! @parameter i_output_mode | <p class="shorttext synchronized" lang="en">Output mode (GUI/string_table)</p>
    METHODS constructor
      IMPORTING
        i_output_mode TYPE zif_sapdev_gw_tool=>ty_output_mode DEFAULT zif_sapdev_gw_tool=>gc_output_mode-gui_output.


  PROTECTED SECTION.
    METHODS:
      build_icfservice_fcat RETURNING VALUE(result) TYPE slis_t_fieldcat_alv,

      retrieve_list_output
        IMPORTING
          i_free        TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(result) TYPE list_string_table,

      itab_to_csv
        IMPORTING
          i_tab         TYPE INDEX TABLE
          i_separator   TYPE clike DEFAULT ';'
        RETURNING
          VALUE(result) TYPE list_string_table.

  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_sapdev_gw_tool IMPLEMENTATION.

  METHOD constructor.
    me->output_mode = i_output_mode.
  ENDMETHOD.

  METHOD zif_sapdev_gw_tool~wipe_client_cache.
    "Do we have this ?
    SELECT SINGLE @abap_true FROM tadir INTO @DATA(exists)
      WHERE pgmid = 'R3TR'
        AND object = 'PROG'
        AND obj_name = '/UI2/INVALIDATE_CLIENT_CACHES'.

    IF sy-subrc NE 0.
      IF me->output_mode = gc_output_mode-gui_output.
        MESSAGE 'Report /UI2/INVALIDATE_CLIENT_CACHES does not exist'(001) TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        APPEND TEXT-001 TO result.
      ENDIF.
      RETURN.
    ENDIF.

    IF i_just_for_username IS INITIAL.
      IF me->output_mode = gc_output_mode-gui_output.
        SUBMIT /ui2/invalidate_client_caches WITH gv_all = abap_true AND RETURN. "#EC CI_SUBMIT
      ELSE.
        SUBMIT /ui2/invalidate_client_caches WITH gv_all = abap_true EXPORTING LIST TO MEMORY AND RETURN. "#EC CI_SUBMIT
        result = retrieve_list_output( ).
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

        result = retrieve_list_output( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD zif_sapdev_gw_tool~wipe_global_cache.

    "Do we have this ?
    SELECT SINGLE @abap_true FROM tadir INTO @DATA(exists)
      WHERE pgmid = 'R3TR'
        AND object = 'PROG'
        AND obj_name = '/UI2/INVALIDATE_GLOBAL_CACHES'.

    IF sy-subrc NE 0.
      IF me->output_mode = gc_output_mode-gui_output.
        MESSAGE 'Report /UI2/INVALIDATE_GLOBAL_CACHES does not exist'(002) TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        APPEND TEXT-002 TO result.
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

      result = retrieve_list_output( ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_sapdev_gw_tool~wipe_odata_meta_cache.

    IF lines(  i_service_ranges ) = 0.
      IF me->output_mode = gc_output_mode-gui_output.
        MESSAGE 'Please select at least one service'(003) TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        APPEND TEXT-003 TO result.
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
          APPEND |ERROR: { service-srv_identifier } | TO result.
          APPEND |  { service-srv_identifier }| TO result.
        ENDIF.
        CONTINUE.
      ELSE.
        IF me->output_mode = gc_output_mode-gui_output.
          WRITE: / icon_okay AS ICON, service-srv_identifier.
        ELSE.
          APPEND |Wiped: { service-srv_identifier }| TO result ##NO_TEXT.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF sy-subrc <> 0.
      DATA(no_hits) = '! NO SERVICES FOUND FOR YOUR SELECTION !'.
      IF me->output_mode = gc_output_mode-gui_output.
        WRITE no_hits.
      ELSE.
        APPEND |{ no_hits }| TO result.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD zif_sapdev_gw_tool~get_show_icf_active.

    cl_icf_service_publication=>get_activate_nodes( IMPORTING it_icf_exchg_pub = DATA(active_services) ).

    IF me->output_mode = gc_output_mode-gui_output.

      "Setup and Display List
      IF i_show_ui5_odata_only = abap_true.
        "Filter on UI5 and OData services by default
        DATA(list_filter) = VALUE slis_t_filter_alv(
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/ui5_ui5/*')
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/opu/*')
        ).
      ENDIF.

      DATA(field_catalog) = build_icfservice_fcat( ).

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY' "#EC SCOPE_OF_VAR
        EXPORTING
          i_structure_name = 'ICF_EXCHG_PUB'
          i_grid_title     = CONV lvc_title( 'Active Services' ) ##NO_TEXT
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
      IF i_show_ui5_odata_only = abap_true.
        "Filter on UI5 and OData services by default
        LOOP AT active_services TRANSPORTING NO FIELDS WHERE ( path NP '/sap/bc/ui5_ui5/*' AND path NP '/sap/opu/*' ). "#EC PREF_LINE_EX
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

  METHOD zif_sapdev_gw_tool~get_show_icf_inactive.
    cl_icf_service_publication=>get_inactive_nodes( IMPORTING et_icf_exchg_pub  = DATA(inactive_services) ).

    IF me->output_mode = gc_output_mode-gui_output.

      "Setup and Display List
      IF i_show_ui5_odata_only = abap_true.
        "Filter on UI5 and OData services by default
        DATA(list_filter) = VALUE slis_t_filter_alv(
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/ui5_ui5/*')
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/opu/*')
        ).
      ENDIF.

      DATA(field_catalog) = build_icfservice_fcat( ).

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'  "#EC SCOPE_OF_VAR
        EXPORTING
          i_structure_name = 'ICF_EXCHG_PUB' ##NO_TEXT
          i_grid_title     = CONV lvc_title( 'Inactive Services' ) ##NO_TEXT
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
      IF i_show_ui5_odata_only = abap_true.
        "Filter on UI5 and OData services by default
        LOOP AT inactive_services TRANSPORTING NO FIELDS WHERE ( path NP '/sap/bc/ui5_ui5/*' AND path NP '/sap/opu/*' ). "#EC PREF_LINE_EX
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
        ct_fieldcat            = result
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    LOOP AT result ASSIGNING FIELD-SYMBOL(<field_meta>).
      CASE <field_meta>-fieldname.
        WHEN 'VHOST' OR 'REF_VHOST' OR 'CREATION_DATE' OR 'CREATION_TIME' OR 'COUNTER'.
          <field_meta>-no_out =  abap_true.
        WHEN 'PATH'.
          <field_meta>-seltext_s = 'Serv/Alias'.
          <field_meta>-seltext_m = <field_meta>-seltext_l = <field_meta>-reptext_ddic = 'Service/Alias'(004). "#EC EQUALS_CHAINING
        WHEN 'REF_PATH'.
          <field_meta>-seltext_s = 'AliasedSrv' .
          <field_meta>-seltext_m = <field_meta>-seltext_l = <field_meta>-reptext_ddic = 'Aliased Service'(005). "#EC EQUALS_CHAINING
        WHEN 'PUBLIC_SERVICE'.
          <field_meta>-seltext_s = <field_meta>-seltext_m = <field_meta>-seltext_l = <field_meta>-reptext_ddic = 'Public'(006). "#EC EQUALS_CHAINING
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
        list_string_ascii  = result
      TABLES
        listobject         = listobject
      EXCEPTIONS
        empty_list         = 1
        list_index_invalid = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
      RETURN.
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

      APPEND csv_line TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD zif_sapdev_gw_tool~calc_app_index.

    "Do we have this ?
    SELECT SINGLE @abap_true FROM tadir INTO @DATA(exists)
      WHERE pgmid = 'R3TR'
        AND object = 'PROG'
        AND obj_name = '/UI5/APP_INDEX_CALCULATE'.

    IF sy-subrc NE 0.
      IF me->output_mode = gc_output_mode-gui_output.
        MESSAGE 'Report /UI5/APP_INDEX_CALCULATE does not exist'(007) TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        APPEND TEXT-007 TO result.
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

      result = retrieve_list_output( ).
    ENDIF.

  ENDMETHOD.

  METHOD zif_sapdev_gw_tool~wipe_odata_meta_cache_token.

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

        "SAPDEV: Custom Code - BEGIN
        IF me->output_mode = gc_output_mode-gui_output.
          MESSAGE 'Backend context tokens have been invalidated successfully'(008) TYPE 'S'.
        ELSE.
          APPEND TEXT-008 TO result.
        ENDIF.

      CATCH cx_root INTO DATA(ex_root).               "#EC NEED_CX_ROOT
        IF me->output_mode = gc_output_mode-gui_output.
          MESSAGE ex_root->get_text( ) TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
        ELSE.
          APPEND ex_root->get_text( ) TO result ##NO_TEXT.
        ENDIF.
    ENDTRY.
    "SAPDEV: Custom Code - END

  ENDMETHOD.

ENDCLASS.
