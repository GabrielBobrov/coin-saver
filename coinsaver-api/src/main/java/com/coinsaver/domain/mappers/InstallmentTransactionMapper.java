package com.coinsaver.domain.mappers;

import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.domain.entities.InstallmentTransaction;
import com.coinsaver.domain.entities.Transaction;

public class InstallmentTransactionMapper {

    public static InstallmentTransaction updateInstallmentTransactionFields(InstallmentTransaction installmentTransaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        installmentTransaction.setAmount(updateTransactionRequestDto.getAmount());
        installmentTransaction.setCategory(updateTransactionRequestDto.getCategory());
        installmentTransaction.setPayDay(updateTransactionRequestDto.getPayDay());
        installmentTransaction.setStatus(updateTransactionRequestDto.getStatus());
        installmentTransaction.setDescription(removeInstallmentFromDescription(updateTransactionRequestDto.getDescription()) + getInstallment(installmentTransaction.getDescription()));
        return installmentTransaction;
    }

    public static InstallmentTransaction updateThisExpense(InstallmentTransaction installmentTransaction, UpdateTransactionRequestDto updateTransactionRequestDto) {
        installmentTransaction.setAmount(updateTransactionRequestDto.getAmount());
        installmentTransaction.setCategory(updateTransactionRequestDto.getCategory());
        installmentTransaction.setPayDay(updateTransactionRequestDto.getPayDay());
        installmentTransaction.setStatus(updateTransactionRequestDto.getStatus());
        installmentTransaction.setDescription(updateTransactionRequestDto.getDescription() + getInstallment(installmentTransaction.getDescription()));
        return installmentTransaction;
    }

    public static InstallmentTransaction updateAllInstallmentTransactions(InstallmentTransaction installmentTransaction,
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

    private static String removeInstallmentFromDescription(String description) {
        int lastIndexOfOpenParenthesis = description.lastIndexOf("(");
        int lastIndexOfCloseParenthesis = description.lastIndexOf(")");
        if (lastIndexOfOpenParenthesis != -1 && lastIndexOfCloseParenthesis != -1) {
            return description.substring(0, lastIndexOfOpenParenthesis).trim();
        }
        return description;
    }

    private static String getInstallment(String description) {
        int lastIndexOfOpenParenthesis = description.lastIndexOf("(");
        int lastIndexOfCloseParenthesis = description.lastIndexOf(")");
        if (lastIndexOfOpenParenthesis != -1 && lastIndexOfCloseParenthesis != -1) {
            return "(" + description.substring(lastIndexOfOpenParenthesis + 1, lastIndexOfCloseParenthesis) + ")";
        }
        return "";
    }
}
