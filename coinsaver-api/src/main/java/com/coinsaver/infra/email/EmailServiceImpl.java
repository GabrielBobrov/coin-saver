package com.coinsaver.infra.email;

import com.coinsaver.core.email.EmailProperties;
import com.coinsaver.domain.entities.EmailMessage;
import com.coinsaver.domain.exceptions.EmailException;
import com.coinsaver.services.email.EmailService;
import jakarta.mail.MessagingException;
import jakarta.mail.internet.MimeMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;


@Service
public class EmailServiceImpl implements EmailService {

    private final JavaMailSender mailSender;
    private final EmailProperties emailProperties;

    public EmailServiceImpl(JavaMailSender mailSender,
                            EmailProperties emailProperties) {
        this.mailSender = mailSender;
        this.emailProperties = emailProperties;
    }

    @Override
    public void sendEmail(EmailMessage emailMessage) {

        try {
            MimeMessage mimeMessage = createMimeMessage(emailMessage);
            mailSender.send(mimeMessage);
        } catch (Exception e) {
            throw new EmailException("Não foi possível enviar e-mail");
        }
    }

    protected MimeMessage createMimeMessage(EmailMessage emailMessage) throws MessagingException {

        MimeMessage mimeMessage = mailSender.createMimeMessage();

        MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, "UTF-8");
        helper.setFrom(emailProperties.getSender());
        helper.setTo(emailMessage.getRecipients().toArray(new String[0]));
        helper.setSubject(emailMessage.getSubject());
        helper.setText(emailMessage.getBody(), true);

        return mimeMessage;
    }
}
