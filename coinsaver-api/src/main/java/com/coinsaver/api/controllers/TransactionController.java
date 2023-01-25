package com.coinsaver.api.controllers;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.services.interfaces.TransactionService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDateTime;
import java.util.List;


@RestController
@RequestMapping(value = "/transactions")
public class TransactionController {

    @Autowired
    private TransactionService transactionService;

    @GetMapping("/{transactionId}")
    @ResponseStatus(HttpStatus.OK)
    public TransactionResponseDto getTransaction(@PathVariable Long transactionId) {

        return transactionService.getTransaction(transactionId);
    }

    @GetMapping("/category/{categoryType}")
    @ResponseStatus(HttpStatus.OK)
    public List<TransactionResponseDto> getTransactionByCategoryType(@PathVariable TransactionCategoryType categoryType, @RequestParam LocalDateTime date) {

        return transactionService.getTransactionByCategory(categoryType, date);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public TransactionResponseDto createTransaction(@RequestBody @Valid TransactionRequestDto transactionRequestDto) {

        return transactionService.createTransaction(transactionRequestDto);
    }

    @GetMapping("/month")
    @ResponseStatus(HttpStatus.OK)
    public MonthlyResponseDto getTransactionsInMonth(@RequestParam LocalDateTime date) {

        return transactionService.getMonthlyTransactions(date);
    }
}
