package com.coinsaver.core.validation.messages;

import java.util.HashMap;
import java.util.Map;


public class ErrorMessages {
    private static final Map<String, String> errorMessages = new HashMap<>();

    static {
        errorMessages.put("USER_ALREADY_EXISTS", "Já existe um usuário cadastrado com o email informado.");
        errorMessages.put("USER_NOT_FOUND", "Não existe nenhum usuário com o id informado.");
        errorMessages.put("INVALID_FIX_EXPENSE", "Não é possível criar uma transação fixa com meses de repetição.");
    }


    public static String getErrorMessage(String key) {
        return errorMessages.get(key);
    }

    public static String userInvalid(String errors) {
        return "Os campos informados para o usuário estão inválidos" + errors;
    }

}
