@description('Deployment environment name.')
@allowed([
  'dev'
  'staging'
  'production'
])
param environment string

@description('Base name prefix for all resources.')
param appName string = 'nordkredit'

@description('Azure region. Sweden Central for GDPR data residency compliance.')
param location string = 'swedencentral'

@description('Azure AD tenant ID for Key Vault access policies.')
param tenantId string

@description('SQL Server administrator login.')
param sqlAdminLogin string

@description('SQL Server administrator password.')
@secure()
param sqlAdminPassword string

@description('App Service Plan SKU.')
param appServiceSkuName string = 'S1'

@description('Azure SQL Database SKU.')
param sqlSkuName string = 'S1'

@description('Azure Functions SKU.')
param functionsSkuName string = 'Y1'

@description('Service Bus SKU.')
param serviceBusSkuName string = 'Standard'

@description('Storage Account SKU.')
param storageSkuName string = 'Standard_LRS'

@description('Log Analytics retention in days.')
param logRetentionInDays int = 90

// Resource name prefix includes environment
var resourcePrefix = '${appName}-${environment}'

// Pre-compute Key Vault URI to avoid circular dependency
var keyVaultNameRaw = replace('${resourcePrefix}-kv', '-', '')
var keyVaultName = length(keyVaultNameRaw) > 24 ? substring(keyVaultNameRaw, 0, 24) : keyVaultNameRaw
var keyVaultUri = 'https://${keyVaultName}${az.environment().suffixes.keyvaultDns}/'

var tags = {
  Project: 'NordKredit'
  Environment: environment
  ManagedBy: 'Bicep'
  Regulation: 'GDPR-DORA-PSD2'
}

// Application Insights and Log Analytics
module appInsights 'modules/app-insights.bicep' = {
  name: 'deploy-app-insights'
  params: {
    appName: resourcePrefix
    location: location
    retentionInDays: logRetentionInDays
    tags: tags
  }
}

// Storage Account (required for Azure Functions runtime)
module storage 'modules/storage-account.bicep' = {
  name: 'deploy-storage'
  params: {
    appName: resourcePrefix
    location: location
    skuName: storageSkuName
    tags: tags
  }
}

// Service Bus for Bankgirot and SWIFT integrations
module serviceBus 'modules/service-bus.bicep' = {
  name: 'deploy-service-bus'
  params: {
    appName: resourcePrefix
    location: location
    skuName: serviceBusSkuName
    tags: tags
  }
}

// Azure SQL Database for GDPR-compliant data storage
module sql 'modules/azure-sql.bicep' = {
  name: 'deploy-sql'
  params: {
    appName: resourcePrefix
    location: location
    sqlAdminLogin: sqlAdminLogin
    sqlAdminPassword: sqlAdminPassword
    skuName: sqlSkuName
    tags: tags
  }
}

// App Service for NordKredit.Api (REST API)
module appService 'modules/app-service.bicep' = {
  name: 'deploy-app-service'
  params: {
    appName: resourcePrefix
    location: location
    skuName: appServiceSkuName
    appInsightsConnectionString: appInsights.outputs.connectionString
    keyVaultUri: keyVaultUri
    tags: tags
  }
}

// Azure Functions for batch processing (NordKredit.Functions)
module functions 'modules/azure-functions.bicep' = {
  name: 'deploy-functions'
  params: {
    appName: resourcePrefix
    location: location
    skuName: functionsSkuName
    appInsightsConnectionString: appInsights.outputs.connectionString
    keyVaultUri: keyVaultUri
    storageConnectionString: '@Microsoft.KeyVault(SecretUri=${keyVaultUri}secrets/StorageConnection)'
    tags: tags
  }
}

// Key Vault for secrets management (depends on App Service and Functions for managed identity access)
module keyVault 'modules/key-vault.bicep' = {
  name: 'deploy-key-vault'
  params: {
    appName: resourcePrefix
    location: location
    tenantId: tenantId
    accessPrincipalIds: [
      appService.outputs.principalId
      functions.outputs.principalId
    ]
    tags: tags
  }
}

// Store storage account connection string in Key Vault
module storageSecret 'modules/key-vault-storage-secret.bicep' = {
  name: 'deploy-storage-secret'
  params: {
    keyVaultName: keyVault.outputs.name
    storageAccountName: storage.outputs.name
    secretName: 'StorageConnection'
  }
}

// Store SQL connection string in Key Vault
module sqlConnectionSecret 'modules/key-vault-secret.bicep' = {
  name: 'deploy-sql-connection-secret'
  params: {
    keyVaultName: keyVault.outputs.name
    secretName: 'NordKreditDb'
    secretValue: '${sql.outputs.connectionStringTemplate}Authentication=Active Directory Default;'
  }
}

// Store Service Bus connection string in Key Vault
module serviceBusConnectionSecret 'modules/key-vault-servicebus-secret.bicep' = {
  name: 'deploy-sb-connection-secret'
  params: {
    keyVaultName: keyVault.outputs.name
    serviceBusNamespaceName: serviceBus.outputs.namespaceName
    secretName: 'ServiceBus'
  }
}

// Outputs
@description('App Service default hostname.')
output apiHostName string = appService.outputs.defaultHostName

@description('Functions App default hostname.')
output functionsHostName string = functions.outputs.defaultHostName

@description('Key Vault URI.')
output keyVaultOutputUri string = keyVault.outputs.uri

@description('SQL Server FQDN.')
output sqlServerFqdn string = sql.outputs.fullyQualifiedDomainName

@description('Application Insights connection string.')
output appInsightsConnectionString string = appInsights.outputs.connectionString
