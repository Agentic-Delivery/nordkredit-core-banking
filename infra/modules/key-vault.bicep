@description('Base name for the Key Vault resources.')
param appName string

@description('Azure region for deployment. Sweden Central for GDPR compliance.')
param location string = 'swedencentral'

@description('Azure AD tenant ID.')
param tenantId string

@description('Principal IDs to grant Key Vault access (App Service, Functions).')
param accessPrincipalIds array = []

@description('Tags applied to all resources.')
param tags object = {}

var keyVaultName = replace('${appName}-kv', '-', '')

resource keyVault 'Microsoft.KeyVault/vaults@2023-07-01' = {
  name: length(keyVaultName) > 24 ? substring(keyVaultName, 0, 24) : keyVaultName
  location: location
  tags: tags
  properties: {
    tenantId: tenantId
    sku: {
      family: 'A'
      name: 'standard'
    }
    enabledForDeployment: false
    enabledForDiskEncryption: false
    enabledForTemplateDeployment: true
    enableSoftDelete: true
    softDeleteRetentionInDays: 90
    enablePurgeProtection: true
    enableRbacAuthorization: false
    accessPolicies: [
      for principalId in accessPrincipalIds: {
        tenantId: tenantId
        objectId: principalId
        permissions: {
          secrets: [
            'get'
            'list'
          ]
        }
      }
    ]
  }
}

@description('Key Vault URI.')
output uri string = keyVault.properties.vaultUri

@description('Key Vault resource ID.')
output id string = keyVault.id

@description('Key Vault name.')
output name string = keyVault.name
