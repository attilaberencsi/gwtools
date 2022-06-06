# Gateway Tools for UI5 / Fiori and OData

## About 
Gateway Helper Tools for Fiori Developers, Application Managers and DevOps colleagues to speed up recurring DevOps tasks in development and quality/test systems from a central place.

### Motivation

Being in this industry for a while as ABAP and Fiori developer, my mind is time to time about how-to simplify admin work of DevOps. Every time I adjust a CDS view / metadata extension, deploy to the backend or transport a new version of a UI5 application, the same activity is to be performed, like wiping caches so that my changes become active.

This tool is not aimed to be used in productive environment, it is rather to support and speed up the development process in development and quality systems.

I gave this tool a format, and reshared here for the community, from where I also got lot of help and support in the past decades.

Note: Wiping specific caches – when it is unnecessary – will slow down the performance of the Fiori Launchpad and result in bad user experience. Brute-force wiping all kind of caches to make changes active without understanding what is being actually wiped is really not a good idea. Have You experienced loading tiles forever already ? To prevent such situations, I put a guide on the selection-screen to provide a simple guidance. You’ll find here also a detailed explanation about the situations where the functions of this report become useful.

## Features
- Wipe Client (SMICM) Cache
- Wipe Global (Auth/Nav) Cache
- Wipe Metadata Cache - Backend + FrontEnd
- Show Active SICF Services
- Show Inactive SICF Services
- Calculate App Index (Apps running in FLP)

All of them are already part of the standard already, but You reach them through multiple steps/clicks and from variety of transactions. The report gives You instant central access to them. I suggest You to look at the source, to get some insight, what was the original standard report or function behind.

## Installation

- Pull this GitHub repository using ABAPGit (Online / offline behind corporate walls)
- Create report ZGW_TOOS_OO and class ZCL_SAPDEV_GW_TOOL in your system. The sources can be found in /src folder, and add manually the text element

The legacy copy/paste version ZGW_TOOLS report is not improved anymore. All new fetures go to the ZGW_TOOLS_OO only.

## Test results
### ATC
- Passed

### Functional
#### Validated on ABAP 1909 successfully

| Software Component | Release | Support Package      | Support Package Level | Description                 |
|--------------------|---------|----------------------|-----------------------|-----------------------------|
| S4FND              | 104     | SAPK-10402INS4FND    | 0002                  | Foundation                  |
| SAP_ABA            | 75E     | SAPK-75E02INSAPABA   | 0002                  | Cross-Application Component |
| SAP_BASIS          | 754     | SAPK-75402INSAPBASIS | 0002                  | SAP Basis Component         |
| SAP_GWFND          | 754     | SAPK-75402INSAPGWFND | 0002                  | SAP Gateway Foundation      |
| SAP_UI             | 754     | SAPK-75404INSAPUI    | 0004                  | User Interface Technology   |


#### Validated on ABAP 750 successfully

| Software Component | Release | Support Package      | Support Package Level | Description                   |
|--------------------|---------|----------------------|-----------------------|-------------------------------|
| SAP_BS_FND         | 748     | SAPK-74816INSAPBSFND | 0016                  | SAP Business Suite Foundation |
| SAP_ABA            | 750     | SAPK-75020INSAPABA   | 0020                  | Cross-Application Component   |
| SAP_BASIS          | 750     | SAPK-75020INSAPBASIS | 0020                  | SAP Basis Component           |
| SAP_GWFND          | 750     | SAPK-75020INSAPGWFND | 0020                  | SAP Gateway Foundation        |
| SAP_UI             | 754     | SAPK-75405INSAPUI    | 0005                  | User Interface Technology     |

## Usage
Check out the detailed description with screenshots at sapdev.eu

https://www.sapdev.eu/gateway-tools-for-ui5-fiori/


Help section guides You to choose the proper action.

A – After deploying from WebIDE Fullstack, Business Application Studio or VSCode to the BSP repository hosting the web application files. Use when the new version of the UI5 application does not appear, because of the new source files are not loaded even after wiping the web browser cache.

B – After You changed PFCG roles, included new target applications in catalogs, adjusted role assignments, but they do not appear on Fiori Launchpad.
Please also validate manually whether the User Comparison is executed in the target system, where the transport request is imported containing the adjusted PFCG role. This can be also one of the reasons of access problems. The validation and comparison can be done manually in transaction PFCG, or executing a mass report in the target system.

C – This symptom become more in the foreground with metadata driven applications. You perform changes to the OData service, but it is not reflected on the UI. You added annotations to the metadata extension file, or new fields to the consumption view. I would like to mention here that, please do not run this feature with * without a good reason. In such case I would immediately take my chainsaw from my drawer my dear as Your colleague :).

Hint: when using CDS entities as reference model in SEGW projects, the generation of the project might required, which has nothing to do with the cache. In case You enhanced a standard CDS view in such setup, You might faced the issue that a value help annotation will never be part of the OData service, because this would need the regeneration of the SEGW project => standard modification.

D – Becomes a handy function when implementing an SAP system for example. You can use this feature to get a list of active services in the development/quality and production systems and do a comparison to discover discrepancies. You can read here more about mass activation of SICF services.

E – When You activate an OData service in the development system using transaction /IWFND/MAINT_SERVICE or using a task list, the service endpoint is created in ICF and activated. The activation does not automatically happen after importing the transport request in the target system. You can discover such endpoints, and activate them in transaction SICF. The same has to be done for UI5 applications. You can use this feature to discover such services, so that end users are not getting the popup, that the Fiori application cannot be loaded, not available, please contact system administrator etc.

F – This is a tiny feature added to option D and E to put a filter on the ICF service list, to show only OData and UI5 application services (which BTW are doubled under node /sap/bc/bsp in transaction SICF always).

H - FLP not only use tokenized/timestamped cache for UI5 application sources, but for OData as well. Metadata and annotations however live a separate lifecycle, their tokens are called Backend Context of the Application Index.

The report **/ui5/upd_odata_metadata_cache** should be scheduled to run at every 1-48 hours (1 is recommended). This results picking up new OData service fields/annotations for example after You adjust a service. But being in a development system, waiting even a single hour is not acceptable. This option executes the report **/ui5/del_odata_metadata_cache** in the background, which does not update but wipes all tokens of OData caches. The update report needs more authorization also in the backend systems, but the delete report not. Use it in development (top most in Test) system only.

Official SAP documentation is as follows - Update report:

*To ensure fast loading times for SAP Fiori apps started from the launchpad, OData metadata (metadata and annotations documents) of SAP Fiori apps is cached on the web browser. The SAP Fiori front-end server manages and persists cache buster tokens to cache OData metadata. During the startup of the launchpad, the tokens are provided to the client to use the correct representation from the underlying web caches*

*Note: To ensure the cache buster tokens are up-to-date, you have to execute report /UI5/UPD_ODATA_METADATA_CACHE periodically. We recommend an hourly execution interval.*
