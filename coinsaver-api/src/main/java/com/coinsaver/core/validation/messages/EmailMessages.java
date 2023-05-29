package com.coinsaver.core.validation.messages;

public class EmailMessages {
    private EmailMessages() { }

    private static final String RECOVER_PASSWORD = "%s, sua senha atual é: %s. Faça login em sua conta e troque a mesma na aba ALTERAR SENHA.";

    public static String getRecoverPasswordMessage(String clientName, String password) {
        return String.format(RECOVER_PASSWORD, clientName, password);
    }

}
