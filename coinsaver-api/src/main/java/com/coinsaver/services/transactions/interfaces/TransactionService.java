package com.coinsaver.services.transactions.interfaces;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.ReceiveTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyChartDivisionResponseDto;
import com.coinsaver.api.dtos.response.MonthlyChartResponseDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.PerformanceResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;

import java.time.LocalDate;
import java.util.List;

public interface TransactionService {
    TransactionResponseDto getTransaction(Long transactionId, TransactionType transactionType);

    List<TransactionResponseDto> getTransactionByCategory(TransactionCategoryType categoryType, LocalDate date);

    void createTransaction(TransactionRequestDto transactionRequestDto);

    MonthlyResponseDto getMonthlyTransactions(LocalDate date);

    UpdateTransactionResponseDto updateTransaction(UpdateTransactionRequestDto transactionRequestDto);

    void payTransaction(PayTransactionRequestDto payTransactionRequestDto);

    void deleteByTransactionId(Long transactionId);

    void receiveTransaction(ReceiveTransactionRequestDto receiveTransactionRequestDto);

    List<MonthlyChartResponseDto> getTransactionsAmountByCategory(LocalDate date);

    List<MonthlyChartDivisionResponseDto> getTransactionsAmountByDivision(LocalDate date, TransactionCategoryType categoryType);

    PerformanceResponseDto getPerformance(LocalDate date);
}
