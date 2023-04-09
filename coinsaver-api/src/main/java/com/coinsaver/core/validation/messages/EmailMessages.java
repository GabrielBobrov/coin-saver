package com.coinsaver.core.validation.messages;

public class EmailMessages {
    private EmailMessages() { }

    private static final String RECOVER_PASSWORD = "%s, sua senha atual é: %s.";

    public static String getRecoverPasswordMessage(String clientName, String password) {
        return String.format(RECOVER_PASSWORD, clientName, password);
    }

}
