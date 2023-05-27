package com.coinsaver.services.transactions;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.ReceiveTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyChartDivisionResponseDto;
import com.coinsaver.api.dtos.response.MonthlyChartResponseDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.MonthlyTransactionResponseDto;
import com.coinsaver.api.dtos.response.PerformanceDivisionsResponseDto;
import com.coinsaver.api.dtos.response.PerformanceEconomyResponseDto;
import com.coinsaver.api.dtos.response.PerformanceMonthlyBalanceResponseDto;
import com.coinsaver.api.dtos.response.PerformanceResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import com.coinsaver.core.enums.UpdateTransactionType;
import com.coinsaver.core.utils.SecurityUtil;
import com.coinsaver.core.validation.messages.ErrorMessages;
import com.coinsaver.domain.entities.Client;
import com.coinsaver.domain.entities.Division;
import com.coinsaver.domain.entities.FixTransaction;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.domain.entities.TransactionBase;
import com.coinsaver.domain.exceptions.BusinessException;
import com.coinsaver.domain.mapper.FixTransactionMapper;
import com.coinsaver.domain.mapper.InstallmentTransactionMapper;
import com.coinsaver.domain.mapper.TransactionMapper;
import com.coinsaver.infra.repositories.DivisionRepository;
import com.coinsaver.infra.repositories.FixTransactionRepository;
import com.coinsaver.infra.repositories.InstallmentTransactionRepository;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.transactions.domain.interfaces.FixTransactionDomainService;
import com.coinsaver.services.transactions.domain.interfaces.InstallmentTransactionDomainService;
import com.coinsaver.services.transactions.domain.interfaces.TransactionDomainService;
import com.coinsaver.services.transactions.interfaces.TransactionService;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.format.TextStyle;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

@Service
public class TransactionServiceImpl implements TransactionService {

    private static final String TRANSACTION_NOT_FOUND = "TRANSACTION_NOT_FOUND";

    private final TransactionRepository transactionRepository;

    private final InstallmentTransactionRepository installmentTransactionRepository;

    private final InstallmentTransactionDomainService installmentTransactionDomainService;

    private final TransactionDomainService transactionDomainService;

    private final FixTransactionDomainService fixTransactionDomainService;

    private final FixTransactionRepository fixTransactionRepository;

    private final InstallmentTransactionMapper installmentTransactionMapper;

    private final FixTransactionMapper fixTransactionMapper;

    private final TransactionMapper transactionMapper;

    private final DivisionRepository divisionRepository;


    public TransactionServiceImpl(TransactionRepository transactionRepository,
                                  InstallmentTransactionRepository installmentTransactionRepository,
                                  InstallmentTransactionDomainService installmentTransactionDomainService,
                                  TransactionDomainService transactionDomainService,
                                  FixTransactionDomainService fixTransactionDomainService,
                                  FixTransactionRepository fixTransactionRepository,
                                  InstallmentTransactionMapper installmentTransactionMapper,
                                  FixTransactionMapper fixTransactionMapper,
                                  TransactionMapper transactionMapper,
                                  DivisionRepository divisionRepository) {
        this.transactionRepository = transactionRepository;
        this.installmentTransactionRepository = installmentTransactionRepository;
        this.installmentTransactionDomainService = installmentTransactionDomainService;
        this.transactionDomainService = transactionDomainService;
        this.fixTransactionDomainService = fixTransactionDomainService;
        this.fixTransactionRepository = fixTransactionRepository;
        this.installmentTransactionMapper = installmentTransactionMapper;
        this.fixTransactionMapper = fixTransactionMapper;
        this.transactionMapper = transactionMapper;
        this.divisionRepository = divisionRepository;
    }

    @Override
    public TransactionResponseDto getTransaction(Long transactionId, TransactionType transactionType) {

        switch (transactionType) {
            case IN_CASH -> {
                Transaction transaction = transactionRepository.findByIdAndTransactionType(transactionId, transactionType)
                        .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage(TRANSACTION_NOT_FOUND)));

                return transaction.convertEntityToResponseDto();
            }
            case FIX -> {
                FixTransaction fixTransaction = fixTransactionRepository.findById(transactionId)
                        .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage(TRANSACTION_NOT_FOUND)));

                return fixTransaction.convertEntityToResponseDto();
            }
            case INSTALLMENT -> {
                InstallmentTransaction installmentTransaction = installmentTransactionRepository.findById(transactionId)
                        .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage(TRANSACTION_NOT_FOUND)));

                return installmentTransaction.convertEntityToResponseDto();
            }
        }
        return null;
    }

    @Override
    public List<TransactionResponseDto> getTransactionByCategory(TransactionCategoryType categoryType, LocalDate date) {

        LocalDate startOfMonth = date.withDayOfMonth(1);
        LocalDate endOfMonth = date.withDayOfMonth(date.lengthOfMonth());
        Client client = SecurityUtil.getClientFromJwt();

        var allTransactions = transactionRepository.findTransactionByPayDayBetweenAndClientAndCategory(startOfMonth, endOfMonth, client, categoryType);

        List<Transaction> transactions = allTransactions
                .stream()
                .filter(t -> t.getTransactionType() == TransactionType.IN_CASH)
                .toList();

        var installmentTransactions = installmentTransactionRepository.findByPayDayBetweenAndTransactions(startOfMonth, endOfMonth, categoryType, client);
        var fixTransactionsEdited = fixTransactionRepository.findFixTransactionByPayDayBetween(startOfMonth, endOfMonth, Boolean.TRUE, allTransactions, categoryType);
        var fixTransactions = fixTransactionRepository.findFixTransactionByEditedFalse(client, categoryType);

        List<Transaction> transactionsEdited = fixTransactionsEdited.stream()
                .map(FixTransaction::getTransaction)
                .toList();

        List<FixTransaction> fixTransactionsNotEdited = fixTransactions.stream()
                .filter(fixTransaction -> !transactionsEdited.contains(fixTransaction.getTransaction()))
                .toList();

        List<FixTransaction> fixTransactionsResult = new ArrayList<>(fixTransactionsEdited);
        fixTransactionsResult.addAll(fixTransactionsNotEdited);

        List<Transaction> transactionsList = new ArrayList<>(transactions);
        transactionsList.addAll(installmentTransactions.stream()
                .map(TransactionBase::convertToTransactionEntity)
                .toList());

        transactionsList.addAll(fixTransactionsResult.stream()
                .map(transaction -> {
                    int monthDifference = getMonthDifference(startOfMonth, transaction.getPayDay());
                    var payday = transaction.getPayDay().plusMonths(monthDifference);
                    transaction.setPayDay(payday);
                    return transaction.convertToTransactionEntity();
                })
                .distinct()
                .toList());
        List<TransactionResponseDto> transactionsResult = new ArrayList<>();

        if (!transactionsList.isEmpty()) {
            transactionsResult.addAll(transactions
                    .stream()
                    .map(transaction -> {
                        TransactionResponseDto responseDto = transactionMapper.fromTransactionToTransactionRequestDto(transaction);
                        responseDto.setTransactionType(TransactionType.IN_CASH);
                        responseDto.setTransactionId(transaction.getId());
                        return responseDto;
                    })
                    .toList());
        }

        if (!installmentTransactions.isEmpty()) {
            transactionsResult.addAll(installmentTransactions
                    .stream()
                    .map(installmentTransaction -> {
                        TransactionResponseDto responseDto = installmentTransactionMapper.fromInstallmentTransactionToTransactionResponseDto(installmentTransaction);
                        responseDto.setTransactionType(TransactionType.INSTALLMENT);
                        return responseDto;
                    })
                    .toList());
        }

        if (!fixTransactionsResult.isEmpty()) {
            transactionsResult.addAll(fixTransactionsResult
                    .stream()
                    .map(fixTransaction -> {
                        TransactionResponseDto responseDto = fixTransactionMapper.fromFixTransactionToTransactionResponseDto(fixTransaction);
                        responseDto.setTransactionType(TransactionType.FIX);
                        return responseDto;
                    })
                    .toList());
        }

        return transactionsResult
                .stream()
                .distinct()
                .toList();
    }

    @Transactional
    @Override
    public void createTransaction(TransactionRequestDto transactionRequestDto) {

        validate(transactionRequestDto);

        Transaction transaction = transactionDomainService.createTransaction(transactionRequestDto);

        if (transactionRequestDto.getRepeat() != null) {
            createInstallmentTransaction(transactionRequestDto, transaction);
        }

        if (Boolean.TRUE.equals(transactionRequestDto.getFixedExpense())) {
            fixTransactionDomainService.createFixTransaction(transactionRequestDto, transaction);
        }
    }

    @Override
    public MonthlyResponseDto getMonthlyTransactions(LocalDate date) {

        LocalDate startOfMonth = date.withDayOfMonth(1);
        LocalDate endOfMonth = date.withDayOfMonth(date.lengthOfMonth());
        Client client = SecurityUtil.getClientFromJwt();

        var allTransactions = transactionRepository.findTransactionByPayDayBetweenAndClient(startOfMonth, endOfMonth, client);

        List<Transaction> transactions = allTransactions
                .stream()
                .filter(t -> t.getTransactionType() == TransactionType.IN_CASH)
                .toList();

        var installmentTransactions = installmentTransactionRepository.findByPayDayBetweenAndTransactions(startOfMonth, endOfMonth, null, client);
        var fixTransactionsEdited = fixTransactionRepository.findFixTransactionByPayDayBetween(startOfMonth, endOfMonth, Boolean.TRUE, allTransactions, null);
        var fixTransactions = fixTransactionRepository.findFixTransactionByEditedFalse(client, null);

        List<Transaction> transactionsEdited = fixTransactionsEdited.stream()
                .map(FixTransaction::getTransaction)
                .toList();

        List<FixTransaction> fixTransactionsNotEdited = fixTransactions.stream()
                .filter(fixTransaction -> !transactionsEdited.contains(fixTransaction.getTransaction()))
                .toList();

        List<FixTransaction> fixTransactionsResult = new ArrayList<>(fixTransactionsEdited);
        fixTransactionsResult.addAll(fixTransactionsNotEdited);

        List<Transaction> allMonthlyTransactions = new ArrayList<>(transactions);
        allMonthlyTransactions.addAll(installmentTransactions.stream()
                .map(TransactionBase::convertToTransactionEntity)
                .toList());

        allMonthlyTransactions.addAll(fixTransactionsResult.stream()
                .map(transaction -> {
                    int monthDifference = getMonthDifference(startOfMonth, transaction.getPayDay());
                    var payday = transaction.getPayDay().plusMonths(monthDifference);
                    transaction.setPayDay(payday);
                    return transaction.convertToTransactionEntity();
                })
                .distinct()
                .toList());

        List<MonthlyTransactionResponseDto> transactionsResult = new ArrayList<>();

        BigDecimal income = BigDecimal.ZERO;
        BigDecimal expense = BigDecimal.ZERO;

        if (!allMonthlyTransactions.isEmpty()) {
            transactionsResult.addAll(transactions
                    .stream()
                    .map(transaction -> {
                        MonthlyTransactionResponseDto responseDto = transaction.convertEntityToMonthlyResponseDto();
                        responseDto.setTransactionType(TransactionType.IN_CASH);
                        responseDto.setTransactionId(transaction.getId());
                        return responseDto;
                    })
                    .toList());

            income = allMonthlyTransactions.stream()
                    .filter(t -> t.getCategory() == TransactionCategoryType.INCOME)
                    .map(Transaction::getAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);

            expense = allMonthlyTransactions.stream()
                    .filter(t -> t.getCategory() == TransactionCategoryType.EXPENSE)
                    .map(Transaction::getAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
        }

        if (!installmentTransactions.isEmpty()) {
            transactionsResult.addAll(installmentTransactions
                    .stream()
                    .map(installmentTransaction -> {
                        MonthlyTransactionResponseDto responseDto = installmentTransaction.convertEntityToMonthlyResponseDto();
                        responseDto.setTransactionType(TransactionType.INSTALLMENT);
                        responseDto.setInstallmentTransactionId(installmentTransaction.getId());
                        return responseDto;
                    })
                    .toList());
        }

        if (!fixTransactionsResult.isEmpty()) {
            transactionsResult.addAll(fixTransactionsResult
                    .stream()
                    .map(fixTransaction -> {
                        MonthlyTransactionResponseDto responseDto = fixTransaction.convertEntityToMonthlyResponseDto();
                        responseDto.setTransactionType(TransactionType.FIX);
                        responseDto.setFixTransactionId(fixTransaction.getId());
                        return responseDto;
                    })
                    .toList());
        }

        BigDecimal monthlyBalance = income.subtract(expense);
        return MonthlyResponseDto.builder()
                .transactions(transactionsResult)
                .monthlyBalance(monthlyBalance)
                .build();
    }

    @Transactional
    @Override
    public UpdateTransactionResponseDto updateTransaction(UpdateTransactionRequestDto updateTransactionRequestDto) {

        validate(updateTransactionRequestDto);

        Transaction transaction = transactionRepository.findById(updateTransactionRequestDto.getTransactionId())
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage(TRANSACTION_NOT_FOUND)));

        UpdateTransactionType updateTransactionType = updateTransactionRequestDto.getUpdateTransactionType();

        switch (updateTransactionType) {
            case ONLY_THIS_EXPENSE -> updateThisExpense(transaction, updateTransactionRequestDto);
            case THIS_EXPENSE_AND_FUTURE_ONES ->
                    updateThisAndFutureExpenses(transaction, updateTransactionRequestDto, updateTransactionType);
            case ALL_EXPENSES -> updateAllExpenses(transaction, updateTransactionRequestDto, updateTransactionType);
        }

        return transaction.convertEntityToUpdateResponseDto();
    }

    @Transactional
    @Override
    public void payTransaction(PayTransactionRequestDto payTransactionRequestDto) {
        switch (payTransactionRequestDto.getTransactionType()) {
            case IN_CASH -> transactionDomainService.payTransaction(payTransactionRequestDto);
            case INSTALLMENT -> installmentTransactionDomainService.payTransaction(payTransactionRequestDto);
            case FIX -> fixTransactionDomainService.payTransaction(payTransactionRequestDto);
        }
    }

    @Transactional
    @Override
    public void deleteByTransactionId(Long transactionId) {

        Transaction transaction = transactionRepository.findById(transactionId)
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        transactionRepository.deleteById(transaction.getId());
    }

    @Transactional
    @Override
    public void receiveTransaction(ReceiveTransactionRequestDto receiveTransactionRequestDto) {
        switch (receiveTransactionRequestDto.getTransactionType()) {
            case IN_CASH -> transactionDomainService.receiveTransaction(receiveTransactionRequestDto);
            case INSTALLMENT -> installmentTransactionDomainService.receiveTransaction(receiveTransactionRequestDto);
            case FIX -> fixTransactionDomainService.receiveTransaction(receiveTransactionRequestDto);
        }
    }

    @Override
    public List<MonthlyChartResponseDto> getTransactionsAmountByCategory(LocalDate date) {
        List<TransactionResponseDto> expenses = this.getTransactionByCategory(TransactionCategoryType.EXPENSE, date);
        List<TransactionResponseDto> incomes = this.getTransactionByCategory(TransactionCategoryType.INCOME, date);

        Map<String, BigDecimal> expenseMap = new HashMap<>();
        Map<String, BigDecimal> incomeMap = new HashMap<>();

        getMapValues(expenses, expenseMap);
        getMapValues(incomes, incomeMap);

        List<MonthlyChartResponseDto> monthlyChartData = new ArrayList<>();

        expenseMap.forEach((categoryName, amount) -> {

            MonthlyChartResponseDto monthlyChartResponseDto = MonthlyChartResponseDto.builder()
                    .categoryName(categoryName)
                    .totalAmount(amount.negate())
                    .build();

            monthlyChartData.add(monthlyChartResponseDto);
        });

        incomeMap.forEach((categoryName, amount) -> {

            MonthlyChartResponseDto monthlyChartResponseDto = MonthlyChartResponseDto.builder()
                    .categoryName(categoryName)
                    .totalAmount(amount)
                    .build();

            monthlyChartData.add(monthlyChartResponseDto);
        });

        return monthlyChartData;
    }

    @Override
    public List<MonthlyChartDivisionResponseDto> getTransactionsAmountByDivision(LocalDate date, TransactionCategoryType categoryType) {

        LocalDate startOfMonth = date.withDayOfMonth(1);
        LocalDate endOfMonth = date.withDayOfMonth(date.lengthOfMonth());
        Client client = SecurityUtil.getClientFromJwt();

        List<Transaction> allTransactions = transactionRepository.findTransactionByPayDayBetweenAndClientAndCategory(startOfMonth, endOfMonth, client, categoryType);

        List<Transaction> transactions = allTransactions
                .stream()
                .filter(t -> t.getTransactionType() == TransactionType.IN_CASH)
                .toList();

        Map<String, BigDecimal> map = new HashMap<>();

        List<InstallmentTransaction> installmentTransactions = installmentTransactionRepository.findByPayDayBetweenAndTransactions(startOfMonth, endOfMonth, categoryType, client);
        List<FixTransaction> fixTransactionsEdited = fixTransactionRepository.findFixTransactionByPayDayBetween(startOfMonth, endOfMonth, Boolean.TRUE, allTransactions, categoryType);
        List<FixTransaction> fixTransactions = fixTransactionRepository.findFixTransactionByEditedFalse(client, categoryType);

        for (InstallmentTransaction it : installmentTransactions) {
            String divisionName = it.getDivision().getName();
            BigDecimal installmentTotal = it.getAmount();
            map.merge(divisionName, installmentTotal, BigDecimal::add);
        }

        for (FixTransaction ft : fixTransactions) {
            String divisionName = ft.getDivision().getName();
            BigDecimal fixTotal = ft.getAmount();
            map.merge(divisionName, fixTotal, BigDecimal::add);
        }

        for (FixTransaction ft : fixTransactionsEdited) {
            String divisionName = ft.getDivision().getName();
            BigDecimal fixTotal = ft.getAmount();
            map.merge(divisionName, fixTotal, BigDecimal::add);
        }

        for (Transaction transaction : transactions) {
            String divisionName = transaction.getDivision().getName();
            BigDecimal fixTotal = transaction.getAmount();
            map.merge(divisionName, fixTotal, BigDecimal::add);
        }

        List<MonthlyChartDivisionResponseDto> resultList = new ArrayList<>();
        for (Map.Entry<String, BigDecimal> entry : map.entrySet()) {
            resultList.add(MonthlyChartDivisionResponseDto.builder()
                    .divisionName(entry.getKey())
                    .totalAmount(entry.getValue())
                    .build());
        }

        return resultList;
    }

    @Override
    public PerformanceResponseDto getPerformance(LocalDate date) {
        PerformanceResponseDto responseDto = new PerformanceResponseDto();

        MonthlyResponseDto currentMonthTransactions = getMonthlyTransactions(date);
        MonthlyResponseDto previousMonthTransactions = getMonthlyTransactions(date.minusMonths(1));

        List<MonthlyTransactionResponseDto> incomesActualMonth = getTransactionsByCategory(currentMonthTransactions, TransactionCategoryType.INCOME);
        List<MonthlyTransactionResponseDto> incomesPreviousMonth = getTransactionsByCategory(previousMonthTransactions, TransactionCategoryType.INCOME);

        List<MonthlyTransactionResponseDto> expensesActualMonth = getTransactionsByCategory(currentMonthTransactions, TransactionCategoryType.EXPENSE);
        List<MonthlyTransactionResponseDto> expensesPreviousMonth = getTransactionsByCategory(previousMonthTransactions, TransactionCategoryType.EXPENSE);

        BigDecimal totalExpensesActualMonth = getTotalAmount(expensesActualMonth);
        BigDecimal totalExpensesPreviousMonth = getTotalAmount(expensesPreviousMonth);

        BigDecimal totalIncomeActualMonth = getTotalAmount(incomesActualMonth);
        BigDecimal totalIncomePreviousMonth = getTotalAmount(incomesPreviousMonth);

        BigDecimal savedPercentageActualMonth = calculatePercentage(totalIncomeActualMonth, totalExpensesActualMonth);
        BigDecimal savedPercentagePreviousMonth = calculatePercentage(totalIncomePreviousMonth, totalExpensesPreviousMonth);

        PerformanceEconomyResponseDto economyResponseDto = PerformanceEconomyResponseDto.builder()
                .actualMonthPercentage(formatPercentage(savedPercentageActualMonth))
                .previousMonthPercentage(formatPercentage(savedPercentagePreviousMonth))
                .build();
        responseDto.setEconomy(economyResponseDto);

        BigDecimal compare = calculatePercentage(currentMonthTransactions.getMonthlyBalance(), previousMonthTransactions.getMonthlyBalance());

        PerformanceMonthlyBalanceResponseDto monthlyBalanceResponseDto = PerformanceMonthlyBalanceResponseDto.builder()
                .actualMonthValue(currentMonthTransactions.getMonthlyBalance())
                .actualMonthName(getMonthDisplayName(date))
                .pastMonthPercentageDifference(formatPercentage(compare))
                .build();
        responseDto.setMonthlyBalance(monthlyBalanceResponseDto);

        String divisionWithMostSpends = getDivisionWithMostSpends(expensesActualMonth);

        PerformanceDivisionsResponseDto divisionsResponseDto = PerformanceDivisionsResponseDto.builder()
                .divisionName(divisionWithMostSpends)
                .divisionSpends(getTotalSpends(divisionWithMostSpends, expensesActualMonth).toString())
                .build();
        responseDto.setDivision(divisionsResponseDto);

        return responseDto;
    }

    private List<MonthlyTransactionResponseDto> getTransactionsByCategory(MonthlyResponseDto monthlyTransactions, TransactionCategoryType category) {
        return monthlyTransactions.getTransactions().stream()
                .filter(t -> t.getCategory() == category)
                .toList();
    }

    private BigDecimal getTotalAmount(List<MonthlyTransactionResponseDto> transactions) {
        return transactions.stream()
                .map(MonthlyTransactionResponseDto::getAmount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    private String formatPercentage(BigDecimal percentage) {
        return percentage.setScale(2, RoundingMode.HALF_UP) + "%";
    }

    private String getMonthDisplayName(LocalDate date) {
        return date.getMonth().getDisplayName(TextStyle.FULL, Locale.getDefault());
    }

    private String getDivisionWithMostSpends(List<MonthlyTransactionResponseDto> expenses) {

        if (expenses.isEmpty()) {
            return null;
        }

        Map<String, BigDecimal> divisionSpends = new HashMap<>();
        for (MonthlyTransactionResponseDto transaction : expenses) {
            String divisionName = transaction.getDivision().getName();
            BigDecimal transactionAmount = transaction.getAmount();
            divisionSpends.put(divisionName, divisionSpends.getOrDefault(divisionName, BigDecimal.ZERO).add(transactionAmount));
        }

        Map.Entry<String, BigDecimal> divisionWithMostSpends = Collections.max(divisionSpends.entrySet(), Map.Entry.comparingByValue());

        return divisionWithMostSpends.getKey();
    }

    private BigDecimal getTotalSpends(String divisionName, List<MonthlyTransactionResponseDto> expenses) {
        BigDecimal totalSpends = BigDecimal.ZERO;
        for (MonthlyTransactionResponseDto transaction : expenses) {
            if (transaction.getDivision().getName().equals(divisionName)) {
                totalSpends = totalSpends.add(transaction.getAmount());
            }
        }
        return totalSpends;
    }

    public static BigDecimal calculatePercentage(BigDecimal incomes, BigDecimal expenses) {
        BigDecimal difference = incomes.subtract(expenses);

        if (incomes.compareTo(BigDecimal.ZERO) == 0) {
            return BigDecimal.ZERO;
        }

        return difference.divide(incomes, 2, RoundingMode.HALF_UP)
                .multiply(BigDecimal.valueOf(100));
    }


    private void getMapValues(List<TransactionResponseDto> transactionResponseDtos, Map<String, BigDecimal> map) {

        for (TransactionResponseDto transactionResponseDto : transactionResponseDtos) {

            String categoryName = transactionResponseDto.getCategory().getName();
            BigDecimal amount = transactionResponseDto.getAmount();

            if (map.containsKey(categoryName)) {
                BigDecimal currentAmount = map.get(categoryName);
                map.put(categoryName, currentAmount.add(amount));
            } else {
                map.put(categoryName, amount);
            }
        }
    }


    private int getMonthDifference(LocalDate startOfMonth, LocalDate payDay) {
        return (startOfMonth.getYear() - payDay.getYear()) * 12 + (startOfMonth.getMonthValue() - payDay.getMonthValue());
    }

    private void updateThisAndFutureExpenses(Transaction transaction,
                                             UpdateTransactionRequestDto updateTransactionRequestDto,
                                             UpdateTransactionType updateTransactionType) {

        if (TransactionType.FIX.equals(updateTransactionRequestDto.getTransactionType()) && updateTransactionRequestDto.getFixTransactionId() != null) {
            throw new BusinessException(ErrorMessages.getErrorMessage("INVALID_FIX_TRANSACTION_UPDATE"));
        }

        transactionDomainService.updateTransactionFields(transaction, updateTransactionRequestDto, updateTransactionType);

        if (TransactionType.INSTALLMENT.equals(updateTransactionRequestDto.getTransactionType())) {
            List<InstallmentTransaction> futureTransactions = findFutureTransactions(updateTransactionRequestDto);
            Integer month = 0;

            for (InstallmentTransaction futureTransaction : futureTransactions) {
                installmentTransactionDomainService.updateInstallmentTransactionFields(futureTransaction, updateTransactionRequestDto, month);
                month++;
            }
        }

    }

    private List<InstallmentTransaction> findFutureTransactions(UpdateTransactionRequestDto updateTransactionRequestDto) {
        InstallmentTransaction installmentTransaction = installmentTransactionRepository.findById(updateTransactionRequestDto.getInstallmentTransactionId())
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage(TRANSACTION_NOT_FOUND)));

        return installmentTransactionRepository.findInstallmentTransactionByPayDayIsGreaterThanEqual(installmentTransaction.getPayDay());
    }

    private void validate(UpdateTransactionRequestDto updateTransactionRequestDto) {
        if (Boolean.TRUE.equals(updateTransactionRequestDto.getFixedExpense()) && Objects.nonNull(updateTransactionRequestDto.getRepeat())) {
            throw new BusinessException(ErrorMessages.getInvalidFixedExpenseMessage("atualizar"));
        }

        if (Boolean.FALSE.equals(updateTransactionRequestDto.getFixedExpense()) && updateTransactionRequestDto.getUpdateTransactionType() == null) {
            throw new BusinessException(ErrorMessages.getErrorMessage("INSTALLMENT_TRANSACTION_TYPE_NULL"));
        }
    }

    private void validate(TransactionRequestDto transactionRequestDto) {
        if (StatusType.NOT_PAID.equals(transactionRequestDto.getStatus()) &&
                TransactionCategoryType.INCOME.equals(transactionRequestDto.getCategory())) {
            throw new BusinessException(ErrorMessages.getErrorMessage("INVALID_STATUS_NOT_PAID_CATEGORY_INCOME"));
        }

        if (StatusType.NOT_RECEIVED.equals(transactionRequestDto.getStatus()) &&
                TransactionCategoryType.EXPENSE.equals(transactionRequestDto.getCategory())) {
            throw new BusinessException(ErrorMessages.getErrorMessage("INVALID_STATUS_NOT_RECEIVED_CATEGORY_EXPENSE"));
        }

        Division division = divisionRepository.findById(transactionRequestDto.getDivisionId())
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        if (TransactionCategoryType.EXPENSE.equals(division.getCategory()) &&
                TransactionCategoryType.INCOME.equals(transactionRequestDto.getCategory()) ||
                TransactionCategoryType.INCOME.equals(division.getCategory()) &&
                        TransactionCategoryType.EXPENSE.equals(transactionRequestDto.getCategory())) {
            throw new BusinessException(ErrorMessages.getErrorMessage("INVALID_DIVISION"));
        }
    }

    private void createInstallmentTransaction(TransactionRequestDto transactionRequestDto, Transaction transaction) {
        int repeat = transactionRequestDto.getRepeat();
        int installment = 1;

        for (int i = 0; i < repeat; i++) {
            installmentTransactionDomainService.createInstallmentTransaction(transactionRequestDto, transaction, installment, i, repeat);
            installment++;
        }
    }

    private void updateInstallmentTransaction(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        int repeat = updateTransactionRequestDto.getRepeat();
        int installment = 1;

        for (int i = 0; i < repeat; i++) {
            InstallmentTransaction installmentTransaction = installmentTransactionMapper.fromUpdateTransactionRequestDtoToInstallmentTransaction(updateTransactionRequestDto);
            installmentTransactionDomainService.updateAllInstallmentTransactions(installmentTransaction, updateTransactionRequestDto, transaction, installment, repeat, i);
            installment++;
        }
    }

    private void updateAllExpenses(Transaction transaction,
                                   UpdateTransactionRequestDto updateTransactionRequestDto,
                                   UpdateTransactionType updateTransactionType) {

        transactionDomainService.updateTransactionFields(transaction, updateTransactionRequestDto, updateTransactionType);

        if (TransactionType.INSTALLMENT.equals(updateTransactionRequestDto.getTransactionType())) {
            if (Objects.isNull(updateTransactionRequestDto.getRepeat())) {
                throw new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_WITHOUT_REPEAT"));
            }
            installmentTransactionRepository.deleteByTransactionId(transaction.getId());
            updateInstallmentTransaction(transaction, updateTransactionRequestDto);
        }

        if (TransactionType.FIX.equals(updateTransactionRequestDto.getTransactionType())) {
            fixTransactionDomainService.updateAllFixTransactions(transaction, updateTransactionRequestDto);
        }
    }

    private void updateThisExpense(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto) {

        switch (transaction.getTransactionType()) {
            case IN_CASH -> transactionDomainService.updateThisTransaction(transaction, updateTransactionRequestDto);
            case INSTALLMENT -> {
                InstallmentTransaction installmentTransaction = installmentTransactionRepository.findById(updateTransactionRequestDto.getInstallmentTransactionId())
                        .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage(TRANSACTION_NOT_FOUND)));

                installmentTransactionDomainService.updateThisExpense(installmentTransaction, updateTransactionRequestDto);
            }
            case FIX -> fixTransactionDomainService.updateFixTransaction(transaction, updateTransactionRequestDto);
        }
    }
}