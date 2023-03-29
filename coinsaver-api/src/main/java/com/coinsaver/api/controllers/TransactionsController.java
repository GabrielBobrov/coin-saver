package com.coinsaver.api.controllers;

import com.coinsaver.api.dtos.request.PayTransactionRequestDto;
import com.coinsaver.api.dtos.request.ReceiveTransactionRequestDto;
import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.api.dtos.response.MonthlyResponseDto;
import com.coinsaver.api.dtos.response.TransactionResponseDto;
import com.coinsaver.api.dtos.response.UpdateTransactionResponseDto;
import com.coinsaver.api.openapi.controller.TransactionsControllerOpenApi;
import com.coinsaver.core.enums.TransactionCategoryType;
import com.coinsaver.core.enums.TransactionType;
import com.coinsaver.services.transactions.interfaces.TransactionService;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import java.time.LocalDate;
import java.util.List;


@RestController
@RequestMapping(value = "/transactions")
public class TransactionsController implements TransactionsControllerOpenApi {

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
    public void createTransaction(@RequestBody @Valid TransactionRequestDto transactionRequestDto) {

        transactionService.createTransaction(transactionRequestDto);
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
    public void payTransaction(@RequestBody @Valid PayTransactionRequestDto payTransactionRequestDto) {

        transactionService.payTransaction(payTransactionRequestDto);
    }

    @DeleteMapping("/{transactionId}")
    @ResponseStatus(HttpStatus.OK)
    public void deleteByTransactionId(@PathVariable Long transactionId) {

        transactionService.deleteByTransactionId(transactionId);
    }

    @PatchMapping("/receive")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void receiveTransaction(@RequestBody @Valid ReceiveTransactionRequestDto receiveTransactionRequestDto) {

        transactionService.receiveTransaction(receiveTransactionRequestDto);
    }
}
