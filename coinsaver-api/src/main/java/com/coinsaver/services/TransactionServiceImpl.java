package com.coinsaver.services;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.validation.messages.ErrorMessages;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.domain.exceptions.BusinessException;
import com.coinsaver.infra.repositories.InstallmentTransactionRepository;
import com.coinsaver.infra.repositories.TransactionRepository;
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

    public TransactionServiceImpl(TransactionRepository transactionRepository, InstallmentTransactionRepository installmentTransactionRepository) {
        this.transactionRepository = transactionRepository;
        this.installmentTransactionRepository = installmentTransactionRepository;
    }

    @Override
    public TransactionResponseDto getTransaction(Long transactionId) {
        var transaction = transactionRepository.findById(transactionId).orElseThrow();

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

        List<TransactionResponseDto> transactionsResult = new ArrayList<>();

        BigDecimal income = BigDecimal.ZERO;
        BigDecimal expense = BigDecimal.ZERO;

        if (!transactions.isEmpty()) {
            transactionsResult.addAll(transactions
                    .stream()
                    .map(Transaction::convertEntityToResponseDto)
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
                    .map(InstallmentTransaction::convertEntityToResponseDto)
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

    @Override
    public TransactionResponseDto updateTransaction(Long transactionId, TransactionRequestDto transactionRequestDto) {

        if (Boolean.TRUE.equals(transactionRequestDto.getFixedExpense()) && transactionRequestDto.getRepeat() > 0) {
            throw new BusinessException(ErrorMessages.getInvalidFixedExpenseMessage("atualizar"));
        }

        var transaction = transactionRepository.findById(transactionId)
                .orElseThrow(() -> new BusinessException("Transaction not found"));

        updateTransactionFields(transaction, transactionRequestDto);
        transactionRepository.save(transaction);

        return transaction.convertEntityToResponseDto();
    }

    private void createInstallmentTransaction(TransactionRequestDto transactionRequestDto, Transaction transaction) {
        int repeat = transactionRequestDto.getRepeat();
        int installment = 1;
        LocalDateTime payDay = transactionRequestDto.getPayDay();

        for (int i = 0; i < repeat; i++) {
            var installmentTransaction = transactionRequestDto.convertDtoToInstallmentTransactionEntity();
            installmentTransaction.setTransaction(transaction);
            installmentTransaction.setDescription(transaction.getDescription() + "(" + installment + "/" + repeat + ")");

            if (i > 0) {
                installmentTransaction.setPayDay(payDay.plusMonths(i));
            }

            installmentTransactionRepository.save(installmentTransaction);
            installment++;
        }
    }

    private void updateTransactionFields(Transaction transaction, TransactionRequestDto transactionRequestDto) {
        transaction.setAmount(transactionRequestDto.getAmount());
        transaction.setCategory(transactionRequestDto.getCategory());
        transaction.setDescription(transactionRequestDto.getDescription());
        transaction.setFixedExpense(transactionRequestDto.getFixedExpense());
        transaction.setPayDay(transactionRequestDto.getPayDay());
        transaction.setRepeat(transactionRequestDto.getRepeat());
        transaction.setStatus(transactionRequestDto.getStatus());
    }
}
