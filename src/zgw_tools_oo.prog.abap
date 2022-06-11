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
SELECTION-SCREEN BEGIN OF BLOCK bo WITH FRAME TITLE mwt.

  PARAMETERS:
    p_wipesm RADIOBUTTON GROUP ro DEFAULT 'X',
    p_unamem TYPE syuname LOWER CASE DEFAULT sy-uname MATCHCODE OBJECT user_comp,
    p_wipesg RADIOBUTTON GROUP ro,
    p_wipeme RADIOBUTTON GROUP ro.

  SELECT-OPTIONS: serv_id FOR /iwfnd/i_med_srh-srv_identifier NO INTERVALS.

  PARAMETERS:
    p_icfact RADIOBUTTON GROUP ro,
    p_icfina RADIOBUTTON GROUP ro,
    p_odui5o AS CHECKBOX,
    p_index  RADIOBUTTON GROUP ro,
    p_repo   TYPE /ui5/ui5_repository_ui,
    p_idxbck RADIOBUTTON GROUP ro.

SELECTION-SCREEN END OF BLOCK bo.


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
    mwt = TEXT-mwt.

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
    DATA(gw_tool) =  NEW zcl_sapdev_gw_tool( i_output_mode = zcl_sapdev_gw_tool=>gc_output_mode-gui_output ).

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
