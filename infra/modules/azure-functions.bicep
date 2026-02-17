@description('Base name for the Azure Functions resources.')
param appName string

@description('Azure region for deployment. Sweden Central for GDPR compliance.')
param location string = 'swedencentral'

@description('App Service Plan SKU for Functions.')
@allowed([
  'Y1'
  'EP1'
  'EP2'
])
param skuName string = 'Y1'

@description('Application Insights connection string.')
@secure()
param appInsightsConnectionString string

@description('Key Vault URI for secret references.')
param keyVaultUri string

@description('Storage account connection string for Functions runtime.')
@secure()
param storageConnectionString string

@description('Tags applied to all resources.')
param tags object = {}

var functionsPlanName = '${appName}-func-plan'
var functionsAppName = '${appName}-func'

resource functionsPlan 'Microsoft.Web/serverfarms@2023-12-01' = {
  name: functionsPlanName
  location: location
  tags: tags
  sku: {
    name: skuName
  }
  properties: {
    reserved: false
  }
}

resource functionsApp 'Microsoft.Web/sites@2023-12-01' = {
  name: functionsAppName
  location: location
  tags: tags
  kind: 'functionapp'
  identity: {
    type: 'SystemAssigned'
  }
  properties: {
    serverFarmId: functionsPlan.id
    httpsOnly: true
    siteConfig: {
      netFrameworkVersion: 'v8.0'
      minTlsVersion: '1.2'
      ftpsState: 'Disabled'
      appSettings: [
        {
          name: 'AzureWebJobsStorage'
          value: storageConnectionString
        }
        {
          name: 'FUNCTIONS_EXTENSION_VERSION'
          value: '~4'
        }
        {
          name: 'FUNCTIONS_WORKER_RUNTIME'
          value: 'dotnet-isolated'
        }
        {
          name: 'APPLICATIONINSIGHTS_CONNECTION_STRING'
          value: appInsightsConnectionString
        }
        {
          name: 'KeyVaultUri'
          value: keyVaultUri
        }
      ]
    }
  }
}

@description('Functions App principal ID for Key Vault access.')
output principalId string = functionsApp.identity.principalId

@description('Functions App default hostname.')
output defaultHostName string = functionsApp.properties.defaultHostName

@description('Functions App resource ID.')
output id string = functionsApp.id

@description('Functions App name.')
output name string = functionsApp.name
