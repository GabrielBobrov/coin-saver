package com.coinsaver.services;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.MonthlyTransactionResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import com.coinsaver.core.enums.StatusType;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import com.coinsaver.core.enums.UpdateInstallmentTransactionType;
import com.coinsaver.core.validation.messages.ErrorMessages;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.domain.exceptions.BusinessException;
import com.coinsaver.infra.repositories.InstallmentTransactionRepository;
import com.coinsaver.infra.repositories.TransactionRepository;
import com.coinsaver.services.domain.interfaces.InstallmentTransactionDomainService;
import com.coinsaver.services.domain.interfaces.TransactionDomainService;
import com.coinsaver.services.interfaces.TransactionService;
import jakarta.transaction.Transactional;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.time.temporal.TemporalAdjusters;
import java.util.ArrayList;
import java.util.List;

@Service
public class TransactionServiceImpl implements TransactionService {

    private final TransactionRepository transactionRepository;

    private final InstallmentTransactionRepository installmentTransactionRepository;

    private final InstallmentTransactionDomainService installmentTransactionDomainService;
    
    private final TransactionDomainService transactionDomainService;

    public TransactionServiceImpl(TransactionRepository transactionRepository,
                                  InstallmentTransactionRepository installmentTransactionRepository,
                                  InstallmentTransactionDomainService installmentTransactionDomainService,
                                  TransactionDomainService transactionDomainService) {
        this.transactionRepository = transactionRepository;
        this.installmentTransactionRepository = installmentTransactionRepository;
        this.installmentTransactionDomainService = installmentTransactionDomainService;
        this.transactionDomainService = transactionDomainService;
    }

    @Override
    public TransactionResponseDto getTransaction(Long transactionId) {
        Transaction transaction = transactionRepository.findById(transactionId)
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        return transaction.convertEntityToResponseDto();
    }

    @Override
    public List<TransactionResponseDto> getTransactionByCategory(TransactionCategoryType categoryType, LocalDateTime date) {

        LocalDateTime startOfMonth = date.with(TemporalAdjusters.firstDayOfMonth());
        LocalDateTime endOfMonth = date.with(TemporalAdjusters.lastDayOfMonth());

        var transactions = transactionRepository.findByCategoryAndPayDayBetweenAndRepeatIsNull(categoryType, startOfMonth, endOfMonth);
        var installmentTransactions = installmentTransactionRepository.findByCategoryAndPayDayBetween(categoryType, startOfMonth, endOfMonth);

        List<TransactionResponseDto> transactionsResult = new ArrayList<>();

        if (!transactions.isEmpty()) {
            transactionsResult.addAll(transactions
                    .stream()
                    .map(Transaction::convertEntityToResponseDto)
                    .toList());
        }

        if (!installmentTransactions.isEmpty()) {
            transactionsResult.addAll(installmentTransactions
                    .stream()
                    .map(it -> it.getTransaction().convertEntityToResponseDto())
                    .toList());
        }

        return transactionsResult;
    }

    @Transactional
    @Override
    public TransactionResponseDto createTransaction(TransactionRequestDto transactionRequestDto) {

        if (Boolean.TRUE.equals(transactionRequestDto.getFixedExpense()) && transactionRequestDto.getRepeat() > 0) {
            throw new BusinessException(ErrorMessages.getInvalidFixedExpenseMessage("criar"));
        }
        var transaction = transactionRepository.save(transactionRequestDto.convertDtoTransactionEntity());

        if (transactionRequestDto.getRepeat() > 0) {
            createInstallmentTransaction(transactionRequestDto, transaction);
        }

        return transaction.convertEntityToResponseDto();
    }

    @Override
    public MonthlyResponseDto getMonthlyTransactions(LocalDateTime date) {

        LocalDateTime startOfMonth = date.with(TemporalAdjusters.firstDayOfMonth());
        LocalDateTime endOfMonth = date.with(TemporalAdjusters.lastDayOfMonth());

        var transactions = transactionRepository.findByPayDayBetweenAndRepeatIsNull(startOfMonth, endOfMonth);
        var installmentTransactions = installmentTransactionRepository.findByPayDayBetween(startOfMonth, endOfMonth);

        List<MonthlyTransactionResponseDto> transactionsResult = new ArrayList<>();

        BigDecimal income = BigDecimal.ZERO;
        BigDecimal expense = BigDecimal.ZERO;

        if (!transactions.isEmpty()) {
            transactionsResult.addAll(transactions
                    .stream()
                    .map(transaction -> {
                        MonthlyTransactionResponseDto responseDto = transaction.convertEntityToMonthlyResponseDto();
                        responseDto.setTransactionType(TransactionType.IN_CASH);
                        return responseDto;
                    })
                    .toList());

            income = transactions.stream()
                    .filter(t -> t.getCategory() == TransactionCategoryType.INCOME)
                    .map(Transaction::getAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);

            expense = transactions.stream()
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
                        return responseDto;
                    })
                    .toList());

            income = income.add(installmentTransactions.stream()
                    .filter(it -> it.getCategory() == TransactionCategoryType.INCOME)
                    .map(InstallmentTransaction::getAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add));

            expense = expense.add(installmentTransactions.stream()
                    .filter(it -> it.getCategory() == TransactionCategoryType.EXPENSE)
                    .map(InstallmentTransaction::getAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add));
        }

        BigDecimal monthlyBalance = income.subtract(expense);
        return MonthlyResponseDto.builder()
                .transactionResponseDtos(transactionsResult)
                .monthlyBalance(monthlyBalance)
                .build();
    }

    @Transactional
    @Override
    public UpdateTransactionResponseDto updateTransaction(Long transactionId, UpdateTransactionRequestDto updateTransactionRequestDto) {

        validate(updateTransactionRequestDto);

        Transaction transaction = transactionRepository.findById(transactionId)
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        if (updateTransactionRequestDto.getRepeat() > 0) {
            UpdateInstallmentTransactionType updateInstallmentTransactionType = updateTransactionRequestDto.getUpdateInstallmentTransactionType();

            switch (updateInstallmentTransactionType) {
                case ONLY_THIS_EXPENSE -> updateThisExpense(transaction, updateTransactionRequestDto);
                case THIS_EXPENSE_AND_FUTURE_ONES ->
                        updateThisAndFutureExpenses(transaction, updateTransactionRequestDto, updateInstallmentTransactionType);
                case ALL_EXPENSES ->
                        updateAllExpenses(transaction, updateTransactionRequestDto, updateInstallmentTransactionType);
            }
        }
        return transaction.convertEntityToUpdateResponseDto();
    }

    @Transactional
    @Override
    public void payTransaction(PayTransactionRequestDto payTransactionRequestDto) {
        switch (payTransactionRequestDto.getTransactionType()) {
            case IN_CASH -> {
                Transaction transaction = transactionRepository.findById(payTransactionRequestDto.getTransactionId())
                        .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

                transaction.setStatus(StatusType.PAID);
                transactionRepository.save(transaction);
            }
            case INSTALLMENT -> {
                InstallmentTransaction installmentTransaction = installmentTransactionRepository.findById(payTransactionRequestDto.getTransactionId())
                        .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

                installmentTransaction.setStatus(StatusType.PAID);
                installmentTransactionRepository.save(installmentTransaction);
            }
        }
    }

    private void updateThisAndFutureExpenses(Transaction transaction,
                                             UpdateTransactionRequestDto updateTransactionRequestDto,
                                             UpdateInstallmentTransactionType updateInstallmentTransactionType) {

        transactionDomainService.updateTransactionFields(transaction, updateTransactionRequestDto, updateInstallmentTransactionType);

        List<InstallmentTransaction> futureTransactions = findFutureTransactions(updateTransactionRequestDto);

        for (InstallmentTransaction futureTransaction : futureTransactions) {
            installmentTransactionRepository.save(installmentTransactionDomainService.updateInstallmentTransactionFields(futureTransaction, updateTransactionRequestDto));
        }
    }

    private List<InstallmentTransaction> findFutureTransactions(UpdateTransactionRequestDto updateTransactionRequestDto) {
        InstallmentTransaction installmentTransaction = installmentTransactionRepository.findById(updateTransactionRequestDto.getInstallmentTransactionId())
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

        return installmentTransactionRepository.findInstallmentTransactionByPayDayIsGreaterThanEqual(installmentTransaction.getPayDay());
    }

    private void validate(UpdateTransactionRequestDto updateTransactionRequestDto) {
        if (Boolean.TRUE.equals(updateTransactionRequestDto.getFixedExpense()) && updateTransactionRequestDto.getRepeat() > 0) {
            throw new BusinessException(ErrorMessages.getInvalidFixedExpenseMessage("atualizar"));
        }

        if (Boolean.FALSE.equals(updateTransactionRequestDto.getFixedExpense()) && updateTransactionRequestDto.getUpdateInstallmentTransactionType() == null) {
            throw new BusinessException(ErrorMessages.getErrorMessage("INSTALLMENT_TRANSACTION_TYPE_NULL"));
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
            InstallmentTransaction installmentTransaction = updateTransactionRequestDto.convertDtoToInstallmentTransactionEntity();
            installmentTransactionDomainService.updateAllInstallmentTransactions(installmentTransaction, updateTransactionRequestDto, transaction, installment, repeat, i);
            installment++;
        }
    }

    private void updateAllExpenses(Transaction transaction,
                                   UpdateTransactionRequestDto updateTransactionRequestDto,
                                   UpdateInstallmentTransactionType updateInstallmentTransactionType) {


        transactionDomainService.updateTransactionFields(transaction, updateTransactionRequestDto, updateInstallmentTransactionType);

        installmentTransactionRepository.deleteByTransactionId(transaction.getId());
        updateInstallmentTransaction(transaction, updateTransactionRequestDto);
    }

    private void updateThisExpense(Transaction transaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        //TODO: ajustar metodo para atualizar transactions
        if (updateTransactionRequestDto.getInstallmentTransactionId() > 0) {
            InstallmentTransaction installmentTransaction = installmentTransactionRepository.findById(updateTransactionRequestDto.getInstallmentTransactionId())
                    .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("TRANSACTION_NOT_FOUND")));

            installmentTransactionDomainService.updateThisExpense(installmentTransaction, updateTransactionRequestDto);
        }
    }
}