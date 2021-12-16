*&---------------------------------------------------------------------*
*& Report zgw_tools
*&---------------------------------------------------------------------*
*& Gateway Helper Tools for Fiori DevOps.
*& Handy tool for Developers, DevOps Colleagues and Application Managers
*& Very plain and simple, and old-school. Designed for copy paste :)
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

SELECTION-SCREEN BEGIN OF BLOCK bh WITH FRAME TITLE ht.
  SELECTION-SCREEN COMMENT /1(79) hl1.
  SELECTION-SCREEN COMMENT /1(79) hl2.
  SELECTION-SCREEN COMMENT /1(79) hl3.
  SELECTION-SCREEN COMMENT /1(79) hl4.
SELECTION-SCREEN END OF BLOCK bh.

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

INITIALIZATION.
  "Set selection-screen texts

  "Parameters
  %_p_wipesm_%_app_%-text = 'A Wipe Client (SMICM) Cache'.  "#EC NOTEXT
  %_p_wipesg_%_app_%-text = 'B Wipe Global (Auth/Nav) Cache'. "#EC NOTEXT
  %_p_wipeme_%_app_%-text = 'C Wipe Metadata Cache - BE+FE'. "#EC NOTEXT

  "Select-options
  %_serv_id_%_app_%-text = 'Wipe Metadata of Service(s)'.   "#EC NOTEXT


  %_p_icfact_%_app_%-text = 'D Show Active SICF Services'.  "#EC NOTEXT
  %_p_icfina_%_app_%-text = 'E Show Inactive SICF Services'. "#EC NOTEXT
  %_p_odui5o_%_app_%-text = 'F Filter on Fiori and OData'.  "#EC NOTEXT

  "Help Text lines
  ht  = 'Help'.
  hl1 = 'WHEN TO USE ?'.
  hl2 = 'A - After deploying UI5 App to BSP repository'.
  hl3 = 'B - After adjusting Roles, Catalogs or Groups'.
  hl4 = 'C - After adjusting CDS Annotations or SEGW'.

  mwt = 'MAGIC WAND'.

START-OF-SELECTION.
  CASE abap_true.


      "WIPE CLIENT CACHES
    WHEN p_wipesm.

      "Do we have this ?
      SELECT SINGLE @abap_true FROM tadir INTO @DATA(exists)
        WHERE pgmid = 'R3TR'
          AND object = 'PROG'
          AND obj_name = '/UI2/INVALIDATE_CLIENT_CACHES'.

      IF sy-subrc NE 0.
        MESSAGE 'Report /UI2/INVALIDATE_CLIENT_CACHES does not exist' TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      SUBMIT /ui2/invalidate_client_caches WITH gv_all = abap_true AND RETURN.



      "WIPE GLOBAL CACHES
    WHEN p_wipesg.

      "Do we have this ?
      SELECT SINGLE @abap_true FROM tadir INTO @exists
        WHERE pgmid = 'R3TR'
          AND object = 'PROG'
          AND obj_name = '/UI2/INVALIDATE_GLOBAL_CACHES'.

      IF sy-subrc NE 0.
        MESSAGE 'Report /UI2/INVALIDATE_GLOBAL_CACHES does not exist' TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      SUBMIT /ui2/invalidate_global_caches
        WITH gv_test = abap_false
        WITH gv_exe = abap_true AND RETURN.



      "WIPE METADATA CACHE
    WHEN p_wipeme.

      IF lines(  serv_id ) = 0.
        MESSAGE 'Please select at least one service' TYPE 'I' DISPLAY LIKE 'E'.
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


      "SHOW ACTIVE SERVICES
    WHEN p_icfact.
      cl_icf_service_publication=>get_activate_nodes( IMPORTING it_icf_exchg_pub = DATA(active_services) ).

      IF p_odui5o = abap_true."Filter on UI5 and OData services by default
        DATA(list_filter) = VALUE slis_t_filter_alv(
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/ui5_ui5/*')
          ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/opu/*')
        ).
      ENDIF.

      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_structure_name = 'ICF_EXCHG_PUB'
          i_grid_title     = CONV lvc_title( 'Active Services' )
          is_layout        = VALUE slis_layout_alv( zebra = abap_true )
          it_filter        = list_filter
        TABLES
          t_outtab         = active_services
        EXCEPTIONS
          program_error    = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN p_icfina.
      cl_icf_service_publication=>get_inactive_nodes( IMPORTING et_icf_exchg_pub  = DATA(inactive_services) ).
      list_filter = VALUE slis_t_filter_alv(
        ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/ui5_ui5/*')
        ( tabname = 'ICF_EXCHG_PUB' fieldname = 'PATH' sign0 = 'I' optio = 'CP' valuf_int = '/sap/bc/opu/*')
      ).


      CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
        EXPORTING
          i_structure_name = 'ICF_EXCHG_PUB'
          i_grid_title     = CONV lvc_title( 'Inactive Services' )
          is_layout        = VALUE slis_layout_alv( zebra = abap_true )
          it_filter        = list_filter
        TABLES
          t_outtab         = inactive_services
        EXCEPTIONS
          program_error    = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.


  ENDCASE.
