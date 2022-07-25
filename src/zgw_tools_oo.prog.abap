*&---------------------------------------------------------------------*
*& Report zgw_tools_oo
*&---------------------------------------------------------------------*
*& Gateway Helper Tool for Fiori DevOps.
*&---------------------------------------------------------------------*
*& Handy for Developers, DevOps Colleagues & Application Managers.
*&---------------------------------------------------------------------*
*& Version Info (YYMMDD): v220611
*& https://github.com/attilaberencsi/gwtools
*& Licence: MIT
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

REPORT zgw_tools_oo.
TABLES /iwfnd/i_med_srh.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Selection-screen
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Execution options (Magic Wand)
SELECTION-SCREEN BEGIN OF BLOCK bo WITH FRAME TITLE TEXT-mwt.

  SELECTION-SCREEN COMMENT /1(79) TEXT-cas.
  SELECTION-SCREEN ULINE.

  PARAMETERS:
    p_wipesm RADIOBUTTON GROUP ro DEFAULT 'X',
    p_unamem TYPE syuname LOWER CASE DEFAULT sy-uname MATCHCODE OBJECT user_comp.

  SELECTION-SCREEN SKIP.
  PARAMETERS:
    p_wipeme RADIOBUTTON GROUP ro.
  SELECT-OPTIONS: serv_id FOR /iwfnd/i_med_srh-srv_identifier NO INTERVALS.


  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT /1(79) TEXT-flp.
  SELECTION-SCREEN ULINE.

  PARAMETERS:
    p_index RADIOBUTTON GROUP ro,
    p_repo  TYPE /ui5/ui5_repository_ui.
  SELECTION-SCREEN SKIP.
  PARAMETERS:
    p_idxbck RADIOBUTTON GROUP ro,
    p_wipesg RADIOBUTTON GROUP ro.


  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN COMMENT /1(50) TEXT-sic.
  SELECTION-SCREEN ULINE.
  PARAMETERS:
    p_icfact RADIOBUTTON GROUP ro,
    p_icfina RADIOBUTTON GROUP ro,
    p_odui5o AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK bo.

SELECTION-SCREEN BEGIN OF BLOCK bg WITH FRAME TITLE TEXT-gid.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_iedm  TYPE c LENGTH 36 LOWER CASE.
    SELECTION-SCREEN PUSHBUTTON 38(12) TEXT-bgi USER-COMMAND gin.
    PARAMETERS: p_oraw  TYPE sysuuid_c32.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS: p_iraw  TYPE sysuuid_c32.
    SELECTION-SCREEN PUSHBUTTON 34(12) TEXT-bgo USER-COMMAND gou.
    PARAMETERS: p_oedm  TYPE c LENGTH 36 LOWER CASE.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK bg.


" Help texts
SELECTION-SCREEN BEGIN OF BLOCK bh WITH FRAME TITLE ht.
  SELECTION-SCREEN COMMENT /1(79) hl1.
  SELECTION-SCREEN COMMENT /1(79) hl2.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 3(4) ico_nono.
    SELECTION-SCREEN COMMENT 7(73) h21.
  SELECTION-SCREEN END OF LINE.
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
  SELECTION-SCREEN COMMENT /1(79) h10.
  SELECTION-SCREEN COMMENT /3(77) h11.
  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 3(4) ico_men.
    SELECTION-SCREEN COMMENT 7(73) hl_men.
  SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN COMMENT /1(79) hl_bctx1.
  SELECTION-SCREEN COMMENT /3(79) hl_bctx2.

SELECTION-SCREEN END OF BLOCK bh.

" Help for browser cache
SELECTION-SCREEN BEGIN OF BLOCK bbc WITH FRAME TITLE tbc.
  SELECTION-SCREEN COMMENT /1(79) bc1.
  SELECTION-SCREEN COMMENT /1(79) bc2.
  SELECTION-SCREEN COMMENT /1(79) bc3.
  SELECTION-SCREEN COMMENT /1(79) bc4.
SELECTION-SCREEN END OF BLOCK bbc.

AT SELECTION-SCREEN.

  DATA(fcode) = sy-ucomm.

  CASE fcode.

    WHEN 'GIN'. "Edm.Guid to RAW16 GUID
      CLEAR fcode.

      NEW zcl_sapdev_gw_tool( )->convert_edm_to_raw16_guid(
                                  EXPORTING
                                    i_edm_guid = CONV #( p_iedm )
                                  RECEIVING
                                    r_raw16_guid = DATA(raw16_guid) ).

      p_oraw = raw16_guid.

      CLEAR raw16_guid.

    WHEN 'GOU'."RAW16 GUID to Edm.Guid
      CLEAR fcode.

      IF strlen( p_iraw ) < zif_sapdev_gw_tool=>co_guid_length-sap.
        CLEAR: p_oedm,raw16_guid.
        RETURN.
      ENDIF.

      TRY.
          raw16_guid = CONV sysuuid_x16( p_iraw ).
        CATCH cx_sy_move_cast_error.
          RETURN.
      ENDTRY.

      NEW zcl_sapdev_gw_tool( )->convert_raw16_to_edm_guid(
                                  EXPORTING
                                    i_raw16_guid = raw16_guid
                                  RECEIVING
                                    r_edm_guid = p_oedm ).

      CLEAR raw16_guid.

  ENDCASE.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Local Helper Class
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
CLASS lcl_gw_tool DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS initialization.
    CLASS-METHODS main.

ENDCLASS.

CLASS lcl_gw_tool IMPLEMENTATION.

  METHOD initialization.
    "Set selection-screen texts

    "Help Text lines
    ht  = TEXT-hth.
    hl1 = TEXT-hl1.
    hl2 = TEXT-hl2.
    h21 = TEXT-h21.
    WRITE icon_message_warning_small AS ICON TO ico_nono.
    hl3 = TEXT-hl3.
    hl4 = TEXT-hl4.
    WRITE icon_message_warning_small AS ICON TO ico_hey.
    hl_hey = TEXT-hlh.
    hl5 = TEXT-hl5.
    hl6 = TEXT-hl6.
    hl7 = TEXT-hl7.
    hl8 = TEXT-hl8.
    WRITE icon_message_warning_small AS ICON TO ico_warn.
    hl9 = TEXT-hl9.
    h10 = TEXT-h10.
    h11 = TEXT-h11.
    WRITE icon_message_warning_small AS ICON TO ico_men.
    hl_men = TEXT-h12.

    hl_bctx1 = TEXT-hbc.
    hl_bctx2 = TEXT-hb2.

    "Help for web browser cache
    tbc = TEXT-tbc.
    bc1 = TEXT-bc1.
    bc2 = TEXT-bc2.
    bc3 = TEXT-bc3.
    bc4 = TEXT-bc4.

  ENDMETHOD.

  METHOD main.
    DATA(gw_tool) = NEW zcl_sapdev_gw_tool( i_output_mode = zcl_sapdev_gw_tool=>gc_output_mode-gui_output ).

    CASE abap_true.

      WHEN p_wipesm.
        gw_tool->wipe_client_cache(  ).

      WHEN p_wipesg.
        gw_tool->wipe_global_cache( ).

      WHEN p_wipeme.
        gw_tool->wipe_odata_meta_cache( i_service_ranges = serv_id[] ).

      WHEN p_icfact.
        gw_tool->get_show_icf_active( i_show_ui5_odata_only = p_odui5o ).

      WHEN p_icfina.
        gw_tool->get_show_icf_inactive( i_show_ui5_odata_only = p_odui5o ).

      WHEN p_index.
        "Calculate UI5 Application Index
        gw_tool->calc_app_index( i_repo = p_repo ).

      WHEN p_idxbck.
        "UI5 Application Index of Backend Context (aka metadata + annotations) Tokens
        gw_tool->wipe_odata_meta_cache_token( ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.


INITIALIZATION.
  lcl_gw_tool=>initialization( ).

START-OF-SELECTION.
  lcl_gw_tool=>main( ).
