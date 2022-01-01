# Gateway Tools for UI5 / Fiori and OData

## About 
Gateway Helper Tools for Fiori Developers, Application Managers and DevOps colleagues to speed up recurring DevOps tasks in development and quality/test systems from a central place.
Very plain, simple and old-school, because it is designed for copy-paste :). 

### Motivation
Being in this industry for a while, my mind is time to time about how-to simplify admin work of DevOps. Every time I adjust a CDS view / metadata extension, deploy or transport a new version of a UI5 application to the backend, the same activity is to be performed, like wiping caches so that my changes become active. This tool is not aimed to be used in productive environment, it is rather to support and speed up the development process in development and quality systems. Wiping specific caches - when it is unnecessary - will slow down the performance of the Fiori Launchpad and result in bad user experience. Brute-force wiping all kind of caches to make changes active without understanding what is being actually wiped is really not a good idea. Have You experienced loading tiles forever already ? To prevent such situations, I put a guide on the selection-screen to provide a simple guidance. I wanted to contribute back to the community, because the  meaningful information I found on the web helped me a lot during the past years. Now I have some time to give that tool a format and share it with You. I hope it will be a helpful tool for You. 

## Features
- Wipe Client (SMICM) Cache
- Wipe Global (Auth/Nav) Cache
- Wipe Metadata Cache - Backend + FrontEnd
- Show Active SICF Services
- Show Inactive SICF Services

All of them are already part of the standard already, but You reach them through multiple steps/clicks and from variety of transactions. The report gives You instant central access to them. I suggest You to look at the source, to get some insight, what was the original standard report or function behind.

## Installation
- Pull this repo using ABAPGit **OR** 
- copy-paste report source: zgw_tools.prog.abap into a GW (FES) or S/4HANA System. 

Not all developers are able to use ABAPGit behind corporate walls, therefore only a single report **ZGW_TOOLS** is to be created in latter option only.


## Test results
### ATC
- Passed

### Functional
- Validated on ABAP 1909 successfully
___

Software Component / Release / Support Package / Support Package Level  Description

___


S4FND               104         SAPK-10402INS4FND     0002                   Foundation

SAP_ABA             75E         SAPK-75E02INSAPABA    0002                   Cross-Application Component

SAP_BASIS           754         SAPK-75402INSAPBASIS  0002                   SAP Basis Component

SAP_GWFND           754         SAPK-75402INSAPGWFND  0002                   SAP Gateway Foundation

SAP_UI              754         SAPK-75404INSAPUI     0004                   User Interface Technology

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
