using '../main.bicep'

param environment = 'production'
param appName = 'nordkredit'
param location = 'swedencentral'
param tenantId = '<azure-ad-tenant-id>'
param sqlAdminLogin = 'nordkreditadmin'
param sqlAdminPassword = '<replace-with-secure-password>'
param appServiceSkuName = 'P1v3'
param sqlSkuName = 'P1'
param functionsSkuName = 'EP1'
param serviceBusSkuName = 'Premium'
param storageSkuName = 'Standard_ZRS'
param logRetentionInDays = 365
