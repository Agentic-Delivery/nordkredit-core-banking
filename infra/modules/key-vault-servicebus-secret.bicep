@description('Name of the existing Key Vault.')
param keyVaultName string

@description('Name of the existing Service Bus namespace.')
param serviceBusNamespaceName string

@description('Name of the authorization rule.')
param authRuleName string = 'AppListenSend'

@description('Secret name in Key Vault.')
param secretName string = 'ServiceBus'

resource keyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
}

resource serviceBusNamespace 'Microsoft.ServiceBus/namespaces@2022-10-01-preview' existing = {
  name: serviceBusNamespaceName
}

resource authRule 'Microsoft.ServiceBus/namespaces/AuthorizationRules@2022-10-01-preview' existing = {
  parent: serviceBusNamespace
  name: authRuleName
}

resource secret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  parent: keyVault
  name: secretName
  properties: {
    value: authRule.listKeys().primaryConnectionString
  }
}

@description('Secret URI (without version).')
output secretUri string = secret.properties.secretUri
