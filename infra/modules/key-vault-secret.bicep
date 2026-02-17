@description('Name of the existing Key Vault.')
param keyVaultName string

@description('Name of the secret.')
param secretName string

@description('Value of the secret.')
@secure()
param secretValue string

resource keyVault 'Microsoft.KeyVault/vaults@2023-07-01' existing = {
  name: keyVaultName
}

resource secret 'Microsoft.KeyVault/vaults/secrets@2023-07-01' = {
  parent: keyVault
  name: secretName
  properties: {
    value: secretValue
  }
}

@description('Secret URI.')
output secretUri string = secret.properties.secretUri
