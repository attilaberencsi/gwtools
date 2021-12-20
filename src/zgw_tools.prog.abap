*&---------------------------------------------------------------------*
*& Report zgw_tools
*&---------------------------------------------------------------------*
*& Gateway Helper for Fiori DevOps.
*& Handy tool for Developers, DevOps Colleagues and Application Managers
*& Very plain, simple and old-school. Designed for copy paste :)
*& Not MVC, just a local helper implemented for separation of model and
*& controller function for a given GW Tool feature or ABAP Event Block.
*&---------------------------------------------------------------------*
*& Validated on ABAP 1909.
*&
*& Software Component  Release     Support Package       Support Package Level  Description
*& ========================================================================================================
*& S4FND               104         SAPK-10402INS4FND     0002                   Foundation
*& SAP_ABA             75E         SAPK-75E02INSAPABA    0002                   Cross-Application Component
*& SAP_BASIS           754         SAPK-75402INSAPBASIS  0002                   SAP Basis Component
*& SAP_GWFND           754         SAPK-75402INSAPGWFND  0002                   SAP Gateway Foundation
*& SAP_UI              754         SAPK-75404INSAPUI     0004                   User Interface Technology
*&---------------------------------------------------------------------*

REPORT zgw_tools.
TABLES /iwfnd/i_med_srh.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Selection-screen
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Execution options (Magic Wand)
SELECTION-SCREEN BEGIN OF BLOCK bo WITH FRAME TITLE mwt.

  PARAMETERS:
    p_wipesm RADIOBUTTON GROUP ro DEFAULT 'X',
    p_wipesg RADIOBUTTON GROUP ro,
    p_wipeme RADIOBUTTON GROUP ro.

  SELECT-OPTIONS: serv_id FOR /iwfnd/i_med_srh-srv_identifier NO INTERVALS.

  PARAMETERS:
    p_icfact RADIOBUTTON GROUP ro,
    p_icfina RADIOBUTTON GROUP ro,
    p_odui5o AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK bo.


" Help texts
SELECTION-SCREEN BEGIN OF BLOCK bh WITH FRAME TITLE ht.
  SELECTION-SCREEN COMMENT /1(79) hl1.
  SELECTION-SCREEN COMMENT /1(79) hl2.
  SELECTION-SCREEN COMMENT /1(79) hl3.
  SELECTION-SCREEN COMMENT /1(79) hl4.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 3(4) ico_hey.
    SELECTION-SCREEN COMMENT 7(73) hl_hey.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN COMMENT /1(79) hl5.
  SELECTION-SCREEN COMMENT /1(79) hl6.
  SELECTION-SCREEN COMMENT /2(78) hl7.
  SELECTION-SCREEN COMMENT /1(79) hl8.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 3(4) ico_warn.
    SELECTION-SCREEN COMMENT 7(73) hl9.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bh.

" Help for browser cache
SELECTION-SCREEN BEGIN OF BLOCK bbc WITH FRAME TITLE tbc.
  SELECTION-SCREEN COMMENT /1(79) bc1.
  SELECTION-SCREEN COMMENT /1(79) bc2.
  SELECTION-SCREEN COMMENT /1(79) bc3.
  SELECTION-SCREEN COMMENT /1(79) bc4.
SELECTION-SCREEN END OF BLOCK bbc.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Local Helper Class
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_gw_tool DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      initialization,
      main,
      wipe_client_cache,
      wipe_global_cache,
      wipe_odata_meta_cache,
      show_icf_active,
      show_icf_inactive,
      build_icfservice_fcat RETURNING VALUE(r_result) TYPE slis_t_fieldcat_alv.

ENDCLASS.

CLASS lcl_gw_tool IMPLEMENTATION.

  METHOD initialization.
    "Set selection-screen texts
    mwt = 'MAGIC WAND'.
    "Parameters
    %_p_wipesm_%_app_%-text = 'A Wipe Client (SMICM) Cache'. "#EC NOTEXT
    %_p_wipesg_%_app_%-text = 'B Wipe Global (Auth/Nav) Cache'. "#EC NOTEXT
    %_p_wipeme_%_app_%-text = 'C Wipe Metadata Cache - BE+FE'. "#EC NOTEXT

    "Select-options
    %_serv_id_%_app_%-text = 'Wipe Metadata of Service(s)'. "#EC NOTEXT

    "Parameters
    %_p_icfact_%_app_%-text = 'D Show Active SICF Services'. "#EC NOTEXT
    %_p_icfina_%_app_%-text = 'E Show Inactive SICF Services'. "#EC NOTEXT
    %_p_odui5o_%_app_%-text = 'F Filter on Fiori and OData'. "#EC NOTEXT

    "Help Text lines
    ht  = 'Help'.                                           "#EC NOTEXT
    hl1 = 'WHEN TO USE ?'.                                  "#EC NOTEXT
    hl2 = 'A - After deploying UI5 App to BSP repository'.  "#EC NOTEXT
    hl3 = 'B - After adjusting Roles, Catalogs or Groups'.  "#EC NOTEXT
    hl4 = 'C - After adjusting CDS Annotations or SEGW'.    "#EC NOTEXT
    WRITE icon_message_warning_small AS ICON TO ico_hey.    "#EC NOTEXT
    hl_hey = 'Select a service and do not kill system performance with *'. "#EC NOTEXT
    hl5 = 'D - Overview on Active SICF services'.           "#EC NOTEXT
    hl6 = 'E - After Import to Quality/Production the services are inactive by default.'. "#EC NOTEXT
    hl7 = '    Use this in the target system(Q/P) to discover such services'. "#EC NOTEXT
    hl8 = 'F - You are looking for Fiori(UI5) apps and OData Services only (D/E)'. "#EC NOTEXT
    WRITE icon_message_warning_small AS ICON TO ico_warn.
    hl9 = 'Service with same name can be found for each UI5 app under /sap/bc/bsp'. "#EC NOTEXT

    "Help for web browser cache
    tbc = 'Wipe web browser cache on local computer'.       "#EC NOTEXT
    bc1 = 'Emptying server cache does not comes with instant results for end users ?'. "#EC NOTEXT
    bc2 = 'In CHROME launch this site to wipe: chrome://settings/clearBrowserData'. "#EC NOTEXT
    bc3 = 'In EDGE launch this site to wipe: edge://history/all'. "#EC NOTEXT
    bc4 = 'In FIREFOX press this key combination to wipe cache: Ctrl+Shift+Del'. "#EC NOTEXT

  ENDMETHOD.



  METHOD main.
    CASE abap_true.

        "WIPE CLIENT CACHES
      WHEN p_wipesm.
        lcl_gw_tool=>wipe_client_cache( ).

        "WIPE GLOBAL CACHES
      WHEN p_wipesg.
        lcl_gw_tool=>wipe_global_cache( ).

        "WIPE METADATA CACHE
      WHEN p_wipeme.
        lcl_gw_tool=>wipe_odata_meta_cache( ).

        "SHOW ACTIVE SERVICES
      WHEN p_icfact.
        lcl_gw_tool=>show_icf_active( ).

        "SHOW INACTIVE SERVICES
      WHEN p_icfina.
        lcl_gw_tool=>show_icf_inactive( ).

    ENDCASE.

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

    SUBMIT /ui2/invalidate_client_caches WITH gv_all = abap_true AND RETURN. "#EC CI_SUBMIT

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

    SUBMIT /ui2/invalidate_global_caches                 "#EC CI_SUBMIT
      WITH gv_test = abap_false
      WITH gv_exe = abap_true AND RETURN.
  ENDMETHOD.

  METHOD wipe_odata_meta_cache.
    IF lines(  serv_id ) = 0.
      MESSAGE 'Please select at least one service' TYPE 'I' DISPLAY LIKE 'E'. "#EC NOTEXT
      RETURN.
    ENDIF.

    SELECT * FROM /iwfnd/i_med_srh INTO TABLE @DATA(services)
    WHERE srv_identifier IN @serv_id.

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
        WRITE: / icon_error_protocol AS ICON, service-srv_identifier.
        WRITE: / '  ', error_text.
        CONTINUE.
      ELSE.
        WRITE: / icon_okay AS ICON, service-srv_identifier.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD show_icf_active.
    cl_icf_service_publication=>get_activate_nodes( IMPORTING it_icf_exchg_pub = DATA(active_services) ).

    IF p_odui5o = abap_true."Filter on UI5 and OData services by default
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

    IF p_odui5o = abap_true."Filter on UI5 and OData services by default
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

ENDCLASS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Setup
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

INITIALIZATION.
  lcl_gw_tool=>initialization( ).


  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Main execution
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

START-OF-SELECTION.
  lcl_gw_tool=>main( ).
