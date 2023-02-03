package com.coinsaver.domain.mappers;

import com.coinsaver.api.dtos.request.UpdateTransactionRequestDto;
import com.coinsaver.core.enums.UpdateInstallmentTransactionType;
import com.coinsaver.domain.entities.Transaction;

public class TransactionMapper {

    public static Transaction updateTransactionFields(Transaction transaction,
                                                      UpdateTransactionRequestDto updateTransactionRequestDto,
                                                      UpdateInstallmentTransactionType updateInstallmentTransactionType) {

        transaction.setAmount(updateTransactionRequestDto.getAmount());
        transaction.setCategory(updateTransactionRequestDto.getCategory());
        transaction.setDescription(updateTransactionRequestDto.getDescription());
        transaction.setFixedExpense(updateTransactionRequestDto.getFixedExpense());
        transaction.setPayDay(updateTransactionRequestDto.getPayDay());
        if (updateInstallmentTransactionType.equals(UpdateInstallmentTransactionType.ALL_EXPENSES)) {
            transaction.setRepeat(updateTransactionRequestDto.getRepeat());
        }
        transaction.setStatus(updateTransactionRequestDto.getStatus());
        return transaction;
    }
}
