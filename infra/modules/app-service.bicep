@description('Base name for the App Service resources.')
param appName string

@description('Azure region for deployment. Sweden Central for GDPR compliance.')
param location string = 'swedencentral'

@description('App Service Plan SKU name.')
@allowed([
  'B1'
  'S1'
  'P1v3'
  'P2v3'
])
param skuName string = 'S1'

@description('Application Insights connection string.')
@secure()
param appInsightsConnectionString string

@description('Key Vault URI for secret references.')
param keyVaultUri string

@description('Tags applied to all resources.')
param tags object = {}

var appServicePlanName = '${appName}-plan'
var appServiceName = '${appName}-api'

resource appServicePlan 'Microsoft.Web/serverfarms@2023-12-01' = {
  name: appServicePlanName
  location: location
  tags: tags
  sku: {
    name: skuName
  }
  properties: {
    reserved: false
  }
}

resource appService 'Microsoft.Web/sites@2023-12-01' = {
  name: appServiceName
  location: location
  tags: tags
  identity: {
    type: 'SystemAssigned'
  }
  properties: {
    serverFarmId: appServicePlan.id
    httpsOnly: true
    siteConfig: {
      netFrameworkVersion: 'v8.0'
      minTlsVersion: '1.2'
      ftpsState: 'Disabled'
      healthCheckPath: '/health/live'
      appSettings: [
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

@description('App Service principal ID for Key Vault access.')
output principalId string = appService.identity.principalId

@description('App Service default hostname.')
output defaultHostName string = appService.properties.defaultHostName

@description('App Service resource ID.')
output id string = appService.id

@description('App Service name.')
output name string = appService.name
