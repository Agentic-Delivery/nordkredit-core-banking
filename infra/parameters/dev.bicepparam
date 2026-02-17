using '../main.bicep'

param environment = 'dev'
param appName = 'nordkredit'
param location = 'swedencentral'
param tenantId = '<azure-ad-tenant-id>'
param sqlAdminLogin = 'nordkreditadmin'
param sqlAdminPassword = '<replace-with-secure-password>'
param appServiceSkuName = 'B1'
param sqlSkuName = 'Basic'
param functionsSkuName = 'Y1'
param serviceBusSkuName = 'Basic'
param storageSkuName = 'Standard_LRS'
param logRetentionInDays = 30
