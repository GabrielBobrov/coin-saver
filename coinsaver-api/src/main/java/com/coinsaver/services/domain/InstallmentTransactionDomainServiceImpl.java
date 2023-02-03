package com.coinsaver.services.domain;

import com.coinsaver.api.dtos.request.TransactionRequestDto;
import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;
import com.coinsaver.infra.repositories.InstallmentTransactionRepository;
import com.coinsaver.services.domain.interfaces.InstallmentTransactionDomainService;

public class InstallmentTransactionDomainServiceImpl implements InstallmentTransactionDomainService {

    private final InstallmentTransactionRepository installmentTransactionRepository;

    public InstallmentTransactionDomainServiceImpl(InstallmentTransactionRepository installmentTransactionRepository) {
        this.installmentTransactionRepository = installmentTransactionRepository;
    }


    @Override
    public InstallmentTransaction updateInstallmentTransactionFields(InstallmentTransaction installmentTransaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        installmentTransaction.setAmount(updateTransactionRequestDto.getAmount());
        installmentTransaction.setCategory(updateTransactionRequestDto.getCategory());
        installmentTransaction.setPayDay(updateTransactionRequestDto.getPayDay());
        installmentTransaction.setStatus(updateTransactionRequestDto.getStatus());
        installmentTransaction.setDescription(removeInstallmentFromDescription(updateTransactionRequestDto.getDescription()) + getInstallment(installmentTransaction.getDescription()));
        return installmentTransaction;
    }

    @Override
    public InstallmentTransaction updateThisExpense(InstallmentTransaction installmentTransaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        installmentTransaction.setAmount(updateTransactionRequestDto.getAmount());
        installmentTransaction.setCategory(updateTransactionRequestDto.getCategory());
        installmentTransaction.setPayDay(updateTransactionRequestDto.getPayDay());
        installmentTransaction.setStatus(updateTransactionRequestDto.getStatus());
        installmentTransaction.setDescription(updateTransactionRequestDto.getDescription() + getInstallment(installmentTransaction.getDescription()));
        return installmentTransaction;
    }

    @Override
    public InstallmentTransaction updateAllInstallmentTransactions(InstallmentTransaction installmentTransaction,
                                                                   UpdateTransactionRequestDto updateTransactionRequestDto,
                                                                   Transaction transaction,
                                                                   Integer installment,
                                                                   Integer repeat,
                                                                   Integer monthQuantity) {

        installmentTransaction.setAmount(updateTransactionRequestDto.getAmount());
        installmentTransaction.setCategory(updateTransactionRequestDto.getCategory());
        installmentTransaction.setDescription(updateTransactionRequestDto.getDescription());
        installmentTransaction.setPayDay(updateTransactionRequestDto.getPayDay());
        installmentTransaction.setStatus(updateTransactionRequestDto.getStatus());
        installmentTransaction.setDescription(transaction.getDescription() + "(" + installment + "/" + repeat + ")");

        if (monthQuantity > 0) {
            installmentTransaction.setPayDay(updateTransactionRequestDto.getPayDay().plusMonths(monthQuantity));
        }

        return installmentTransaction;
    }

    @Override
    public void createInstallmentTransaction(TransactionRequestDto transactionRequestDto, Transaction transaction, Integer installment, Integer monthQuantity, Integer repeat) {
        var installmentTransaction = transactionRequestDto.convertDtoToInstallmentTransactionEntity();
        installmentTransaction.setTransaction(transaction);
        installmentTransaction.setDescription(transactionRequestDto.getDescription() + "(" + installment + "/" + repeat + ")");

        if (monthQuantity > 0) {
            installmentTransaction.setPayDay(transactionRequestDto.getPayDay().plusMonths(monthQuantity));
        }

        installmentTransactionRepository.save(installmentTransaction);
    }

    private String removeInstallmentFromDescription(String description) {
        int lastIndexOfOpenParenthesis = description.lastIndexOf("(");
        int lastIndexOfCloseParenthesis = description.lastIndexOf(")");
        if (lastIndexOfOpenParenthesis != -1 && lastIndexOfCloseParenthesis != -1) {
            return description.substring(0, lastIndexOfOpenParenthesis).trim();
        }
        return description;
    }

    private String getInstallment(String description) {
        int lastIndexOfOpenParenthesis = description.lastIndexOf("(");
        int lastIndexOfCloseParenthesis = description.lastIndexOf(")");
        if (lastIndexOfOpenParenthesis != -1 && lastIndexOfCloseParenthesis != -1) {
            return "(" + description.substring(lastIndexOfOpenParenthesis + 1, lastIndexOfCloseParenthesis) + ")";
        }
        return "";
    }
}