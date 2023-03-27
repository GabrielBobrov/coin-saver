export const environment = {

  api: {
    hostBackend: 'http://localhost:8080',

    transactionsControllerBackend: '/transactions',

    backendEndpoints: {
      getAllTransactions: '',
      getTransaction: '',
      getTransactionsInMonth: '/month',
      getTransactionByCategoryType: '/category',

      createTransaction: '',
      updateTransaction: '',

      updateTransactionPatch: '/pay',

    }
  }
}
