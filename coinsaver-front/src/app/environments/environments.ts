export const environment = {

  api: {
    hostBackend: 'http://localhost:8080',

    transactionsControllerBackend: '/transactions',
    transactionsBackendEndpoints: {
      getAllTransactions: '',
      getTransaction: '',
      getTransactionByCategoryType: '/category',
      createTransaction: '',
      getTransactionsInMonth: '/month',
      updateTransaction: '',
      updateTransactionPatch: '/pay',
      deleteByTransactionId: '',
      getTransactionsAmountByCategory: '/chart/category',
      getTransactionsAmountByDivision: '/chart/division',
    },

    divisionsControllerBackend: '/divisions',
    divisionsBackendEndpoints: {
      getDivisionByCategoryType: '/category',
      getDivisionById: '',
    },

    authenticationControllerBackend: '/auth',
    authenticationBackendEndpoints: {
      register: '/register',
      authenticate: '/authenticate',
    }
  }
}
