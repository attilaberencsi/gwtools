*&---------------------------------------------------------------------*
*& Report zgw_tools_oo
*&---------------------------------------------------------------------*
*& Gateway Helper Tool for Fiori DevOps.
*&---------------------------------------------------------------------*
*& Handy for Developers, DevOps Colleagues & Application Managers.
*&---------------------------------------------------------------------*
*& Author: Attila Berencsi, sapdev.eu
*& Version Info (YYMMDD): v220123
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
    p_odui5o AS CHECKBOX.

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
    CLASS-METHODS: initialization,
      main.

ENDCLASS.

CLASS lcl_gw_tool IMPLEMENTATION.

  METHOD initialization.
    "Set selection-screen texts
    mwt = 'MAGIC WAND'.
    "Parameters
    %_p_wipesm_%_app_%-text = 'A Wipe Client (SMICM) Cache'. "#EC NOTEXT
    %_p_unamem_%_app_%-text = 'For User'.                   "#EC NOTEXT
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
    h21 = 'Clear Your username - which slows down others - when necessary only'. "#EC NOTEXT
    WRITE icon_message_warning_small AS ICON TO ico_nono.   "#EC NOTEXT
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
    DATA(gw_tool) =  NEW zcl_sapdev_gw_tool( i_output_mode = zcl_sapdev_gw_tool=>gui_output ).

    CASE abap_true.

        "WIPE CLIENT CACHES
      WHEN p_wipesm.
        gw_tool->wipe_client_cache(  ).

        "WIPE GLOBAL CACHES
      WHEN p_wipesg.
        gw_tool->wipe_global_cache( ).

        "WIPE METADATA CACHE
      WHEN p_wipeme.
        gw_tool->wipe_odata_meta_cache( i_service_ranges = serv_id[] ).

        "SHOW ACTIVE SERVICES
      WHEN p_icfact.
        gw_tool->get_show_icf_active( i_show_ui5_odata_only = p_odui5o ).

        "SHOW INACTIVE SERVICES
      WHEN p_icfina.
        gw_tool->get_show_icf_inactive( i_show_ui5_odata_only = p_odui5o ).

    ENDCASE.

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
