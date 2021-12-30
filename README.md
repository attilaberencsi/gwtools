# Gateway Tools for UI5 / Fiori

## About 
Gateway Helper Tools for Fiori Developers, Application Managers and DevOps colleagues to speed up recurring DevOps tasks in development and quality/test systems from a central place.
Very plain, simple and old-school, because it is designed for copy-paste :). 

### Motivation
djust a CDS view / metadata extension, deploy or transport a new version of a UI5 application to the backend, the same activity is to be performed, like wiping caches so that my changes become active. This tool is not meant to be used in productive environment, it is rather to support and speed up the development process in development and quality systems. Wiping specific caches when it is unnecessary , will slow down the performance of the Fiori Launchpad and result in bad user experience. I saw in the past lot of times, that developers, consultants, application managers brute-force wiping all kind of caches to make their changes active without being understanding what they actually wiping, because they do not understand the mechanism behind. Therefore I put a guide on the selection-screen to provide a simple usage guidance, to avoid such situations.

## Features
- Wipe Client (SMICM) Cache
- Wipe Global (Auth/Nav) Cache
- Wipe Metadata Cache - Backend + FrontEnd
- Show Active SICF Services
- Show Inactive SICF Services

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
