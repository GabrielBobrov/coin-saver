export const environment = {

  api: {
    hostBackend: 'http://localhost:8080',

    transactionsControllerBackend: '/transactions',

    backendEndpoints: {
      getTransactions: '',
      getTransactionsById: '/{transactionId}',
      getTransactionsMonth: '/month',
      getTransactionsCategoryByCategoryType: '/category/{categoryType}',

      postTransactions: '',
      putTransactions: '',
    }
  }
}
