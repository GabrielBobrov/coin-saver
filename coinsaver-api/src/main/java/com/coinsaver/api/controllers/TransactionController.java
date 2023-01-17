package com.coinsaver.api.controllers;

import com.coinsaver.api.dtos.TransactionDto;
import com.coinsaver.services.interfaces.TransactionService;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;


@RestController
@RequestMapping(value = "/transactions")
public class TransactionController {

    @Autowired
    private TransactionService transactionService;

    @GetMapping("/{transactionId}")
    public TransactionDto getTransaction(@PathVariable Long transactionId) {

        return transactionService.getTransaction(transactionId);
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    public TransactionDto createTransaction(@RequestBody @Valid TransactionDto transactionDto) {

        return transactionService.createTransaction(transactionDto);
    }
}
