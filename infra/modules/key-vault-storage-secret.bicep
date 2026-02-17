@description('Name of the existing Key Vault.')
param keyVaultName string

@description('Name of the existing Storage Account.')
param storageAccountName string

@description('Secret name in Key Vault.')
param secretName string = 'StorageConnection'

resource keyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
}

resource storageAccount 'Microsoft.Storage/storageAccounts@2023-05-01' existing = {
  name: storageAccountName
}

resource secret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  parent: keyVault
  name: secretName
  properties: {
    value: 'DefaultEndpointsProtocol=https;AccountName=${storageAccount.name};EndpointSuffix=${environment().suffixes.storage};AccountKey=${storageAccount.listKeys().keys[0].value}'
  }
}

@description('Secret URI (without version).')
output secretUri string = secret.properties.secretUri
