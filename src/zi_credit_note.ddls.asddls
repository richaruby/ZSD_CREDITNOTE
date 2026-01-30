@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Interface view'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_CREDIT_NOTE 
as select from I_BillingDocument as a 
left outer join  ztb_credit_note as b on a.BillingDocument = b.billingdocument
{
    key a.BillingDocument,
        a.CreationDate,
        b.base64,
        b.base64_3,
        b.m_ind
}
