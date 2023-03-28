package com.coinsaver.api.controllers;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import com.coinsaver.api.openapi.controller.TransactionControllerOpenApi;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import com.coinsaver.services.interfaces.TransactionService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;


@RestController
@RequestMapping(value = "/transactions")
public class TransactionsController implements TransactionControllerOpenApi {

    private final TransactionService transactionService;

    public TransactionsController(TransactionService transactionService) {
        this.transactionService = transactionService;
    }

    @GetMapping("/{transactionId}")
    @ResponseStatus(HttpStatus.OK)
    public TransactionResponseDto getTransaction(@PathVariable Long transactionId, @RequestParam TransactionType transactionType) {

        return transactionService.getTransaction(transactionId, transactionType);
    }

    @GetMapping("/category/{categoryType}")
    @ResponseStatus(HttpStatus.OK)
    public List<TransactionResponseDto> getTransactionByCategoryType(@PathVariable TransactionCategoryType categoryType, @RequestParam LocalDate date) {

        return transactionService.getTransactionByCategory(categoryType, date);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public TransactionResponseDto createTransaction(@RequestBody @Valid TransactionRequestDto transactionRequestDto) {

        return transactionService.createTransaction(transactionRequestDto);
    }

    @GetMapping("/month")
    @ResponseStatus(HttpStatus.OK)
    public MonthlyResponseDto getTransactionsInMonth(@RequestParam LocalDate date) {

        return transactionService.getMonthlyTransactions(date);
    }

    @PutMapping
    @ResponseStatus(HttpStatus.OK)
    public UpdateTransactionResponseDto updateTransaction(@RequestBody @Valid UpdateTransactionRequestDto transactionRequestDto) {

        return transactionService.updateTransaction(transactionRequestDto);
    }

    @PatchMapping("/pay")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void updateTransaction(@RequestBody @Valid PayTransactionRequestDto payTransactionRequestDto) {

        transactionService.payTransaction(payTransactionRequestDto);
    }
}
