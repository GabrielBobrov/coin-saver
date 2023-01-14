package com.coinsaver.api.controllers;

import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.services.TransactionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Optional;

@RestController
@RequestMapping(value = "/transactions")
public class TransactionController {

    @Autowired
    private TransactionService transactionService;

    @GetMapping("/{transactionId}")
    public Optional<Transaction> getTransaction ( @PathVariable Long transactionId) {

        return transactionService.getTransaction(transactionId);
    }
}
