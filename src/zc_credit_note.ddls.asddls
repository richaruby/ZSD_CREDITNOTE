@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Consumption View'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
@UI: {
    headerInfo: {
        typeName: 'Billing Document',
        typeNamePlural: 'Billing Document'
    },
    presentationVariant: [
        {
            sortOrder: [
                {
                    by: 'BillingDocument',
                    direction: #DESC
                }
            ]
        }
    ]
}

define root view entity ZC_CREDIT_NOTE 
provider contract transactional_query
  as projection on ZI_CREDIT_NOTE
{

    
      @UI.facet: [{ id : 'BillingNo',
        purpose: #STANDARD,
        type: #IDENTIFICATION_REFERENCE,
        label: 'Manufacturer Details',
         position: 10 }]


      @UI.lineItem:       [{ position: 10, label: 'BillingDocument' },{ type: #FOR_ACTION , dataAction: 'ZPRINT', label: 'Generate Print'}]
      @UI.identification: [{ position: 10, label: 'BillingDocument' }]
      @UI.selectionField: [{ position: 10 }]
  key BillingDocument,

      @UI.lineItem:       [{ position: 20, label: 'CreationDate' }]
      @UI.identification: [{ position: 20, label: 'CreationDate' }]
      CreationDate,
      base64,
      base64_3,
      m_ind
}
