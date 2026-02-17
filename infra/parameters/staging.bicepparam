using '../main.bicep'

param environment = 'staging'
param appName = 'nordkredit'
param location = 'swedencentral'
param tenantId = '<azure-ad-tenant-id>'
param sqlAdminLogin = 'nordkreditadmin'
param sqlAdminPassword = '<replace-with-secure-password>'
param appServiceSkuName = 'S1'
param sqlSkuName = 'S1'
param functionsSkuName = 'Y1'
param serviceBusSkuName = 'Standard'
param storageSkuName = 'Standard_LRS'
param logRetentionInDays = 90
