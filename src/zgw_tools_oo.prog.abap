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
*& Validated on ABAP onPremise 2022 sp1.
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
" Selection-screen Definition
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
INCLUDE zgw_tools_oo_sel.


AT SELECTION-SCREEN.
  DATA(fcode) = sy-ucomm.

  CASE fcode.

    WHEN 'GIN'. " Edm.Guid to RAW16 GUID
      CLEAR fcode.

      NEW zcl_sapdev_gw_tool( )->convert_edm_to_raw16_guid( EXPORTING i_edm_guid   = CONV #( p_iedm )
                                                            RECEIVING r_raw16_guid = DATA(raw16_guid) ).

      p_oraw = raw16_guid.

      CLEAR raw16_guid.

    WHEN 'GOU'. " RAW16 GUID to Edm.Guid
      CLEAR fcode.

      IF strlen( p_iraw ) < zif_sapdev_gw_tool=>co_guid_length-sap.
        CLEAR: p_oedm,
               raw16_guid.
        RETURN.
      ENDIF.

      TRY.
          raw16_guid = CONV sysuuid_x16( p_iraw ).
        CATCH cx_sy_move_cast_error.
          RETURN.
      ENDTRY.

      NEW zcl_sapdev_gw_tool( )->convert_raw16_to_edm_guid( EXPORTING i_raw16_guid = raw16_guid
                                                            RECEIVING r_edm_guid   = p_oedm ).

      CLEAR raw16_guid.

  ENDCASE.

  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  " Local Helper Class
  """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  INCLUDE zgw_tools_oo_sel_cl.


INITIALIZATION.
  lcl_gw_tool=>initialization( ).

START-OF-SELECTION.
  lcl_gw_tool=>main( ).
