"! <p class="shorttext synchronized" lang="en">GateWay Tools</p>
"! <p>Version Info (YYMMDD): <strong>v220611</strong><p>
"! <p>https://github.com/attilaberencsi/gwtools<p>
"! <p>Licence: MIT<p>
INTERFACE zif_sapdev_gw_tool
  PUBLIC .

  TYPES:
    BEGIN OF ty_srv_id_range,
      sign   TYPE c LENGTH 1,
      option TYPE c LENGTH 2,
      low    TYPE /iwfnd/med_mdl_srg_identifier,
      high   TYPE /iwfnd/med_mdl_srg_identifier,
    END OF ty_srv_id_range.

  TYPES ty_srv_id_ranges TYPE STANDARD TABLE OF ty_srv_id_range.

  TYPES:
    ty_output_mode TYPE i.

  CONSTANTS:
    BEGIN OF gc_output_mode,
      no_output  TYPE ty_output_mode VALUE 0, "Not yet supported
      gui_output TYPE ty_output_mode VALUE 1,
      string_tab TYPE ty_output_mode VALUE 2,
    END OF gc_output_mode.

  CONSTANTS:
    BEGIN OF co_guid_length,
      sap TYPE i VALUE 32,
      edm TYPE i VALUE 36,
    END OF co_guid_length.

  "! <p class="shorttext synchronized" lang="en">Submit report /UI2/INVALIDATE_CLIENT_CACHES</p>
  "!
  "! @parameter i_just_for_username | <p class="shorttext synchronized" lang="en">Wipe Cache for this user only</p>
  "! @parameter result | <p class="shorttext synchronized" lang="en">Plain Log Output</p>
  METHODS wipe_client_cache
    IMPORTING
      i_just_for_username TYPE syuname OPTIONAL
    RETURNING
      VALUE(result)       TYPE list_string_table.

  "! <p class="shorttext synchronized" lang="en">/UI2/INVALIDATE_GLOBAL_CACHES</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en">Plain Log Output</p>
  METHODS wipe_global_cache
    RETURNING
      VALUE(result) TYPE list_string_table.

  "! <p class="shorttext synchronized" lang="en">Triggers Cleanup for MetaData Cache</p>
  "! <p>Calls the function of the SAP Gateway Client transaction: Metadata -> Cleanup Cache</p>
  "!
  "! @parameter i_service_ranges | <p class="shorttext synchronized" lang="en">Service Names</p>
  "! @parameter result | <p class="shorttext synchronized" lang="en">Plain Log Output</p>
  METHODS wipe_odata_meta_cache
    IMPORTING
      i_service_ranges TYPE ty_srv_id_ranges
    RETURNING
      VALUE(result)    TYPE list_string_table.

  "! <p class="shorttext synchronized" lang="en">Invalidate all $metadata+annotation cache tokens-all clients</p>
  "! <p>Executes the single method used in report /UI5/DEL_ODATA_METADATA_CACHE without the message statement</p>
  "!
  "! @parameter result | <p class="shorttext synchronized" lang="en">Plain Log Output</p>
  METHODS wipe_odata_meta_cache_token
    RETURNING
      VALUE(result) TYPE list_string_table.

  "! <p class="shorttext synchronized" lang="en">Calculation of SAPUI5 Application Index</p>
  "! <p>Submits report /UI5/APP_INDEX_CALCULATE</p>
  "! @parameter i_repo | <p class="shorttext synchronized" lang="en">BSP App Name</p>
  "! @parameter result | <p class="shorttext synchronized" lang="en">Plain Log Output</p>
  METHODS calc_app_index
    IMPORTING
      i_repo        TYPE /ui5/ui5_repository_ui OPTIONAL
    RETURNING
      VALUE(result) TYPE list_string_table.

  "! <p class="shorttext synchronized" lang="en">Show active ICF services</p>
  "!
  "! @parameter i_show_ui5_odata_only | <p class="shorttext synchronized" lang="en">Only UI5 App EndPoints</p>
  "! @parameter e_services | <p class="shorttext synchronized" lang="en">Service list</p>
  "! @parameter e_output | <p class="shorttext synchronized" lang="en">Plain Log Output</p>
  METHODS get_show_icf_active                      "#EC NUM_OUTPUT_PARA
    IMPORTING
      i_show_ui5_odata_only TYPE abap_bool OPTIONAL
    EXPORTING
      e_services            TYPE icf_exchg_pub_ttyp
      e_output              TYPE list_string_table.

  "! <p class="shorttext synchronized" lang="en">Show active ICF services</p>
  "!
  "! @parameter i_show_ui5_odata_only | <p class="shorttext synchronized" lang="en">Only UI5 App EndPoints</p>
  "! @parameter e_services | <p class="shorttext synchronized" lang="en">Service list</p>
  "! @parameter e_output | <p class="shorttext synchronized" lang="en">Plain Log Output</p>
  METHODS get_show_icf_inactive                    "#EC NUM_OUTPUT_PARA
    IMPORTING
      i_show_ui5_odata_only TYPE abap_bool OPTIONAL
    EXPORTING
      e_services            TYPE icf_exchg_pub_ttyp
      e_output              TYPE list_string_table.


  "! <p class="shorttext synchronized" lang="en">Convert SAP RAW16 GUID/UUID to Edm.Guid</p>
  "!
  "! @parameter i_raw16_guid | <p class="shorttext synchronized" lang="en">sysuuid_x</p>
  "! @parameter r_edm_guid | <p class="shorttext synchronized" lang="en">Edm.Guid</p>
  METHODS convert_raw16_to_edm_guid
    IMPORTING
      i_raw16_guid      TYPE sysuuid_x
    RETURNING
      VALUE(r_edm_guid) TYPE string.


  "! <p class="shorttext synchronized" lang="en">Convert Edm.Guid to SAP RAW16 GUID/UUID</p>
  "!
  "! @parameter i_edm_guid | <p class="shorttext synchronized" lang="en">Edm.Guid</p>
  "! @parameter r_raw16_guid | <p class="shorttext synchronized" lang="en">sysuuid_x</p>
  METHODS convert_edm_to_raw16_guid
    IMPORTING
      i_edm_guid          TYPE string
    RETURNING
      VALUE(r_raw16_guid) TYPE sysuuid_x.


ENDINTERFACE.
