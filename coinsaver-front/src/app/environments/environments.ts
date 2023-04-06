export const environment = {

  api: {
    hostBackend: 'http://localhost:8080',

    transactionsControllerBackend: '/transactions',

    backendEndpoints: {
      getAllTransactions: '',

      getTransaction: '',
      getTransactionByCategoryType: '/category',

      createTransaction: '',

      getTransactionsInMonth: '/month',

      updateTransaction: '',

      updateTransactionPatch: '/pay',

    }
  }
}
