*&---------------------------------------------------------------------*
*& Include zgw_tools_oo_cl - Gateway Tools - Local Classes
*&---------------------------------------------------------------------*
CLASS lcl_gw_tool DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS initialization.
    CLASS-METHODS main.
    CLASS-METHODS f4_odata_v4_srv_cache.

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

      WHEN p_wipem4.
        DATA(error_text) = gw_tool->wipe_odata_meta_cache_v4(
                               i_group_id    = p_srvgrp
                               i_service_key = VALUE #( repository_id   = p_srvrep
                                                        service_id      = p_srvid4
                                                        service_version = p_srvve4    ) ).
        IF error_text IS NOT INITIAL.
          MESSAGE error_text TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          MESSAGE 'Metadata and Annotation Model/Text Cache wiped successfully' TYPE 'S'.
        ENDIF.

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

  METHOD f4_odata_v4_srv_cache.
    SELECT * FROM ZI_SAPDEV_V4_Cache INTO TABLE @DATA(g_v4caches).

    g_f4_field_mapping_srv4 = VALUE #( ( fldname = 'F0001' dyfldname  = 'P_SRVGRP' )
                                       ( fldname = 'F0002' dyfldname  = 'P_SRVREP' )
                                       ( fldname = 'F0003' dyfldname  = 'P_SRVID4' )
                                       ( fldname = 'F0004' dyfldname  = 'P_SRVVE4' ) ).

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'SERVICEID'
        dynpprog        = sy-cprog
        dynpnr          = sy-dynnr
        dynprofield     = 'P_SRVID4'
        window_title    = 'V4 Cache'
        value_org       = 'S'
      TABLES
        value_tab       = g_v4caches
        return_tab      = g_f4_field_return_srv4
        dynpfld_mapping = g_f4_field_mapping_srv4
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    TRY.
        p_srvgrp = g_f4_field_return_srv4[ 1 ]-fieldval.
        p_srvrep = g_f4_field_return_srv4[ 2 ]-fieldval.
        p_srvve4 = g_f4_field_return_srv4[ 4 ]-fieldval.

      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
