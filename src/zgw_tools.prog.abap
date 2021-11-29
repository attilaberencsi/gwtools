*&---------------------------------------------------------------------*
*& Report zgw_tools
*&---------------------------------------------------------------------*
*& Gateway Helper Tools for Fiori Developers, Application Managers.
*& Very plain and simple, and old-school. Designed for copy paste :)
*&---------------------------------------------------------------------*
REPORT zgw_tools.

SELECTION-SCREEN BEGIN OF BLOCK bh WITH FRAME TITLE ht.
  SELECTION-SCREEN COMMENT /1(79) hl1.
  SELECTION-SCREEN COMMENT /1(79) hl2.
  SELECTION-SCREEN COMMENT /1(79) hl3.
SELECTION-SCREEN END OF BLOCK bh.

PARAMETERS:
  p_wipesm RADIOBUTTON GROUP ro DEFAULT 'X',
  p_wipeme RADIOBUTTON GROUP ro.
PARAMETERS:
  p_srv    TYPE /iwfnd/ui_service_name,
  p_srvver TYPE /iwfnd/ui_service_version.

"p_model TYPE /iwfnd/med_mdl_identifier MATCHCODE OBJECT /iwfnd/sh_model.


INITIALIZATION.
  "Set selection-screen texts

  "Parameters
  %_p_wipesm_%_app_%-text = 'A - Wipe Client (SMICM) Cache'. "#EC NOTEXT
  %_p_wipeme_%_app_%-text = 'B - Wipe Metadata Cache - BE+FE'. "#EC NOTEXT

  "Help Text lines
  ht  = 'Help'.
  hl1 = 'WHEN TO USE WHICH OPTION ?'.
  hl2 = 'A - After deploying UI5 App to BSP repository'.
  hl3 = ''.


START-OF-SELECTION.
  CASE abap_true.

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


    WHEN p_wipeme.

      /iwfnd/cl_sutil_moni=>cleanup_metadata_cache(
        EXPORTING
          iv_mode            = 'A'
          iv_multi_origin    = abap_true
          iv_namespace       = '/SAP/'
          iv_service_name    = p_srv
          iv_service_version = p_srvver
        IMPORTING
          ev_error_text      = DATA(error_text)
      ).

      IF error_text IS NOT INITIAL.
        MESSAGE error_text  TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.


  ENDCASE.
