@description('Base name for the Service Bus resources.')
param appName string

@description('Azure region for deployment. Sweden Central for GDPR compliance.')
param location string = 'swedencentral'

@description('Service Bus SKU.')
@allowed([
  'Basic'
  'Standard'
  'Premium'
])
param skuName string = 'Standard'

@description('Tags applied to all resources.')
param tags object = {}

var namespaceName = '${appName}-sb'

resource serviceBusNamespace 'Microsoft.ServiceBus/namespaces@2022-10-01-preview' = {
  name: namespaceName
  location: location
  tags: tags
  sku: {
    name: skuName
    tier: skuName
  }
  properties: {
    minimumTlsVersion: '1.2'
  }
}

resource bankgirotQueue 'Microsoft.ServiceBus/namespaces/queues@2022-10-01-preview' = {
  parent: serviceBusNamespace
  name: 'bankgirot-payments'
  properties: {
    maxDeliveryCount: 3
    deadLetteringOnMessageExpiration: true
    defaultMessageTimeToLive: 'P1D'
    lockDuration: 'PT1M'
    requiresDuplicateDetection: true
    duplicateDetectionHistoryTimeWindow: 'PT10M'
  }
}

resource swiftQueue 'Microsoft.ServiceBus/namespaces/queues@2022-10-01-preview' = {
  parent: serviceBusNamespace
  name: 'swift-payments'
  properties: {
    maxDeliveryCount: 3
    deadLetteringOnMessageExpiration: true
    defaultMessageTimeToLive: 'P1D'
    lockDuration: 'PT1M'
    requiresDuplicateDetection: true
    duplicateDetectionHistoryTimeWindow: 'PT10M'
  }
}

resource bankgirotDeadLetterQueue 'Microsoft.ServiceBus/namespaces/queues@2022-10-01-preview' = {
  parent: serviceBusNamespace
  name: 'bankgirot-payments-dlq'
  properties: {
    maxDeliveryCount: 10
    defaultMessageTimeToLive: 'P7D'
    lockDuration: 'PT5M'
  }
}

resource swiftDeadLetterQueue 'Microsoft.ServiceBus/namespaces/queues@2022-10-01-preview' = {
  parent: serviceBusNamespace
  name: 'swift-payments-dlq'
  properties: {
    maxDeliveryCount: 10
    defaultMessageTimeToLive: 'P7D'
    lockDuration: 'PT5M'
  }
}

resource listenSendRule 'Microsoft.ServiceBus/namespaces/AuthorizationRules@2022-10-01-preview' = {
  parent: serviceBusNamespace
  name: 'AppListenSend'
  properties: {
    rights: [
      'Listen'
      'Send'
    ]
  }
}

@description('Service Bus namespace ID.')
output namespaceId string = serviceBusNamespace.id

@description('Service Bus namespace name.')
output namespaceName string = serviceBusNamespace.name

@description('Service Bus authorization rule resource ID.')
output authRuleId string = listenSendRule.id
