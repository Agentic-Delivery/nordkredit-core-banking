@description('Base name for the Application Insights resources.')
param appName string

@description('Azure region for deployment. Sweden Central for GDPR compliance.')
param location string = 'swedencentral'

@description('Log Analytics workspace retention in days.')
@minValue(30)
@maxValue(730)
param retentionInDays int = 90

@description('Tags applied to all resources.')
param tags object = {}

var workspaceName = '${appName}-law'
var appInsightsName = '${appName}-ai'

resource logAnalyticsWorkspace 'Microsoft.OperationalInsights/workspaces@2023-09-01' = {
  name: workspaceName
  location: location
  tags: tags
  properties: {
    sku: {
      name: 'PerGB2018'
    }
    retentionInDays: retentionInDays
  }
}

resource appInsights 'Microsoft.Insights/components@2020-02-02' = {
  name: appInsightsName
  location: location
  tags: tags
  kind: 'web'
  properties: {
    Application_Type: 'web'
    WorkspaceResourceId: logAnalyticsWorkspace.id
    IngestionMode: 'LogAnalytics'
    publicNetworkAccessForIngestion: 'Enabled'
    publicNetworkAccessForQuery: 'Enabled'
    RetentionInDays: retentionInDays
  }
}

@description('Application Insights connection string.')
output connectionString string = appInsights.properties.ConnectionString

@description('Application Insights instrumentation key.')
output instrumentationKey string = appInsights.properties.InstrumentationKey

@description('Application Insights resource ID.')
output id string = appInsights.id

@description('Log Analytics workspace ID.')
output workspaceId string = logAnalyticsWorkspace.id
