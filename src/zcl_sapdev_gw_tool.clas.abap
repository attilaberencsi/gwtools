"! <p class="shorttext synchronized" lang="en">GateWay Tools</p>
CLASS zcl_sapdev_gw_tool DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "TYPES ty_output_mode TYPE c LENGTH 10.

    TYPES:
      BEGIN OF ty_srv_id_range,
        sign   TYPE c LENGTH 1,
        option TYPE c LENGTH 2,
        low    TYPE /iwfnd/med_mdl_srg_identifier,
        high   TYPE /iwfnd/med_mdl_srg_identifier,
      END OF ty_srv_id_range.

    TYPES ty_srv_id_ranges TYPE STANDARD TABLE OF ty_srv_id_range.

    TYPES:
      BEGIN OF ENUM te_output_mode,
        no_output,
        gui_output,
        string_tab,
      END OF ENUM te_output_mode.

    DATA:
      output_mode  TYPE te_output_mode READ-ONLY.

    METHODS:
      constructor
        IMPORTING
          i_output_mode TYPE te_output_mode DEFAULT zcl_sapdev_gw_tool=>gui_output,

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

      show_icf_active
        IMPORTING
          i_show_ui5_odata_only TYPE abap_bool OPTIONAL,

      show_icf_inactive
        IMPORTING
          i_show_ui5_odata_only TYPE abap_bool OPTIONAL.


  PROTECTED SECTION.
    METHODS:
      build_icfservice_fcat RETURNING VALUE(r_result) TYPE slis_t_fieldcat_alv,

      retrieve_list_output
        IMPORTING
          i_free          TYPE abap_bool DEFAULT abap_true
        RETURNING
          VALUE(r_output) TYPE list_string_table.


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
      MESSAGE 'Report /UI2/INVALIDATE_CLIENT_CACHES does not exist' TYPE 'I' DISPLAY LIKE 'E'. "#EC NOTEXT
      RETURN.
    ENDIF.

    IF i_just_for_username IS INITIAL.
      IF me->output_mode = gui_output.
        SUBMIT /ui2/invalidate_client_caches WITH gv_all = abap_true AND RETURN. "#EC CI_SUBMIT
      ELSE.
        SUBMIT /ui2/invalidate_client_caches WITH gv_all = abap_true EXPORTING LIST TO MEMORY AND RETURN. "#EC CI_SUBMIT
        r_output = retrieve_list_output( ).
      ENDIF.
    ELSE.
      IF me->output_mode = gui_output.
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
      MESSAGE 'Report /UI2/INVALIDATE_GLOBAL_CACHES does not exist' TYPE 'I' DISPLAY LIKE 'E'. "#EC NOTEXT
      RETURN.
    ENDIF.

    IF me->output_mode = gui_output.
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
      MESSAGE 'Please select at least one service' TYPE 'I' DISPLAY LIKE 'E'. "#EC NOTEXT
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
        IF me->output_mode = gui_output.
          WRITE: / icon_error_protocol AS ICON, service-srv_identifier.
          WRITE: / '  ', error_text.
        ELSE.
          APPEND |ERROR: { service-srv_identifier } | TO r_output.
          APPEND |  { service-srv_identifier }| TO r_output.
        ENDIF.
        CONTINUE.
      ELSE.
        IF me->output_mode = gui_output.
          WRITE: / icon_okay AS ICON, service-srv_identifier.
        ELSE.
          APPEND |Wiped: { service-srv_identifier }| TO r_output.
        ENDIF.
      ENDIF.

    ENDLOOP.
    IF sy-subrc <> 0.
      DATA(no_hits) = '! NO SERVICES FOUND FOR YOUR SELECTION !'.
      IF me->output_mode = gui_output.
        WRITE no_hits.
      ELSE.
        APPEND |{ no_hits }| TO r_output.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD show_icf_active.
    cl_icf_service_publication=>get_activate_nodes( IMPORTING it_icf_exchg_pub = DATA(active_services) ).

    IF i_show_ui5_odata_only = abap_true."Filter on UI5 and OData services by default
      DATA(list_filter) = VALUE slis_t_filter_alv(
        ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/ui5_ui5/*')
        ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/opu/*')
      ).
    ENDIF.

    "Setup and Display List
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
  ENDMETHOD.

  METHOD show_icf_inactive.
    cl_icf_service_publication=>get_inactive_nodes( IMPORTING et_icf_exchg_pub  = DATA(inactive_services) ).

    IF i_show_ui5_odata_only = abap_true."Filter on UI5 and OData services by default
      DATA(list_filter) = VALUE slis_t_filter_alv(
        ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/ui5_ui5/*')
        ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/opu/*')
      ).
    ENDIF.

    "Setup and Display List
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

    IF i_free = abap_true.
      CALL FUNCTION 'LIST_FREE_MEMORY'.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
