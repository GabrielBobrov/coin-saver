export const environment = {
  api: {
    hostBackend: 'http://localhost:8080',
    // hostBackend: 'https://coin-saver-production.up.railway.app',

    transactionsControllerBackend: '/transactions',
    transactionsBackendEndpoints: {
      getAllTransactions: '',
      getTransaction: '',
      getTransactionByCategoryType: '/category',
      createTransaction: '',
      getTransactionsInMonth: '/month',
      updateTransaction: '',
      updateTransactionPatchPay: '/pay',
      updateTransactionPatchReceive: '/receive',
      deleteByTransactionId: '',
      getTransactionsAmountByCategory: '/chart/category',
      getPerformance: '/month/performance',
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
    },

    clientsControllerBackend: '/clients',
    clientsBackendEndpoints: {
      recoverPassword: '/recover-password',
      changePassword: '/change-password',
    },
  },
};
