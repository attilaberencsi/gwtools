@AbapCatalog.viewEnhancementCategory: [ #NONE ]

@AccessControl.authorizationCheck: #NOT_REQUIRED

@EndUserText.label: 'OData V4 Metadata and Annotation Cache'

@Metadata.ignorePropagatedAnnotations: true

@ObjectModel.usageType: { serviceQuality: #X, sizeCategory: #S, dataClass: #MIXED }

define view entity ZI_SAPDEV_V4_Cache
  as select distinct from /iwbep/l_v4_cac  as V4Cache

    inner join            /iwbep/i_v4_msga as SrvGroupServices on  V4Cache.repository_id = SrvGroupServices.repository_id
                                                               and V4Cache.service_id    = SrvGroupServices.service_id

{
  key SrvGroupServices.group_id as GroupId,
  key V4Cache.repository_id     as RepositoryId,
  key V4Cache.service_id        as ServiceId,
  key V4Cache.service_version   as ServiceVersion
}
