package com.coinsaver.core.validation.messages;

import java.util.HashMap;
import java.util.Map;


public class ErrorMessages {
    private ErrorMessages() { }
    private static final Map<String, String> messages = new HashMap<>();

    static {
        messages.put("USER_ALREADY_EXISTS", "Já existe um usuário cadastrado com o email informado.");
        messages.put("USER_NOT_FOUND", "Não existe nenhum usuário com o id informado.");
        messages.put("TRANSACTION_NOT_FOUND", "Transação não encontrada.");
        messages.put("INSTALLMENT_TRANSACTION_TYPE_NULL", "Informe um installmentTransactionType.");
        messages.put("TRANSACTION_WITH_REPEAT", "Não é possível alterar uma transação que possui parcelamentos, altere os parcelamentos.");
        messages.put("INVALID_FIX_TRANSACTION_UPDATE", "Não é possível atualizar transações futuras de uma transação fixa.");
        messages.put("PAY_INCOME_TRANSACTION", "Não é possível pagar uma transação que nào é uma despesa.");
        messages.put("INVALID_STATUS_NOT_PAID_CATEGORY_INCOME", "Não é possível criar uma entrada com status não pago.");
        messages.put("INVALID_STATUS_NOT_RECEIVED_CATEGORY_EXPENSE", "Não é possível criar uma despesa com status não recebido.");
        messages.put("INVALID_DIVISION", "Divisão inválida para categoria da transação.");
        messages.put("DIVISION_NOT_FOUND", "Divisão não encontrada.");
    }


    public static String getErrorMessage(String key) {
        return messages.get(key);
    }

    public static String userInvalid(String errors) {
        return "Os campos informados para o usuário estão inválidos" + errors;
    }

    private static final String ERROR_MESSAGE_FIXED_EXPENSE = "Não é possível %s uma transação fixa com meses de repetição.";

    public static String getInvalidFixedExpenseMessage(String action) {
        return String.format(ERROR_MESSAGE_FIXED_EXPENSE, action);
    }

}
