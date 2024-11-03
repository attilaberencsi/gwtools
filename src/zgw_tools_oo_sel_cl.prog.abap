*&---------------------------------------------------------------------*
*& Include zgw_tools_oo_sel_cl - Gateway Tools - Local Classes
*&---------------------------------------------------------------------*
CLASS lcl_gw_tool DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS initialization.
    CLASS-METHODS main.

ENDCLASS.

CLASS lcl_gw_tool IMPLEMENTATION.
  METHOD initialization.
    " Set selection-screen texts

    " Help Text lines
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

    " Help for web browser cache
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
        gw_tool->wipe_client_cache( p_unamem ).

      WHEN p_wipesg.
        gw_tool->wipe_global_cache( ).

      WHEN p_wipeme.
        gw_tool->wipe_odata_meta_cache( i_service_ranges = serv_id[] ).

      WHEN p_icfact.
        gw_tool->get_show_icf_active( i_show_ui5_odata_only = p_odui5o ).

      WHEN p_icfina.
        gw_tool->get_show_icf_inactive( i_show_ui5_odata_only = p_odui5o ).

      WHEN p_index.
        " Calculate UI5 Application Index
        gw_tool->calc_app_index( i_repo = p_repo ).

      WHEN p_idxbck.
        " UI5 Application Index of Backend Context (aka metadata + annotations) Tokens
        gw_tool->wipe_odata_meta_cache_token( ).

    ENDCASE.
  ENDMETHOD.
ENDCLASS.
