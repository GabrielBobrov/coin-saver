package com.coinsaver.services.email;

import com.coinsaver.domain.entities.EmailMessage;

public interface EmailService {

    void sendEmail(EmailMessage emailMessage);

}
