package com.coinsaver.services.authentication;


import com.coinsaver.api.dtos.request.AuthenticationRequestDto;
import com.coinsaver.api.dtos.request.ChangePasswordRequestDto;
import com.coinsaver.api.dtos.request.RegisterRequestDto;
import com.coinsaver.api.dtos.response.AuthenticationResponseDto;
import com.coinsaver.core.enums.Role;
import com.coinsaver.core.enums.TokenType;
import com.coinsaver.core.utils.SecurityUtil;
import com.coinsaver.core.validation.messages.EmailMessages;
import com.coinsaver.core.validation.messages.ErrorMessages;
import com.coinsaver.domain.entities.Client;
import com.coinsaver.domain.entities.EmailMessage;
import com.coinsaver.domain.entities.Token;
import com.coinsaver.domain.exceptions.BusinessException;
import com.coinsaver.infra.repositories.ClientRepository;
import com.coinsaver.infra.repositories.TokenRepository;
import com.coinsaver.services.authentication.interfaces.AuthenticationService;
import com.coinsaver.services.authentication.interfaces.JwtService;
import com.coinsaver.services.email.EmailService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.jasypt.encryption.pbe.StandardPBEStringEncryptor;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Collections;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthenticationServiceImpl implements AuthenticationService {
    private final ClientRepository clientRepository;
    private final TokenRepository tokenRepository;
    private final StandardPBEStringEncryptor encryptor;
    private final JwtService jwtService;
    private final AuthenticationManager authenticationManager;
    private final EmailService emailService;
    private final PasswordEncoder passwordEncoder;


    public AuthenticationResponseDto register(RegisterRequestDto request) {
        var client = clientRepository.findByEmail(request.getEmail());

        if (client.isPresent())
            throw new BusinessException(ErrorMessages.getErrorMessage("USER_ALREADY_EXISTS"));

        var user = Client.builder()
                .name(request.getName())
                .email(request.getEmail())
                .password(passwordEncoder.encode(request.getPassword()))
                .passwordVerification(encryptor.encrypt(request.getPassword()))
                .role(Role.USER)
                .build();

        var savedUser = clientRepository.save(user);
        var jwtToken = jwtService.generateToken(user);

        saveUserToken(savedUser, jwtToken);

        return AuthenticationResponseDto.builder()
                .token(jwtToken)
                .build();
    }

    public AuthenticationResponseDto authenticate(AuthenticationRequestDto request) {

        authenticationManager
                .authenticate(new UsernamePasswordAuthenticationToken(request.getEmail(), request.getPassword()));

        var user = clientRepository.findByEmail(request.getEmail())
                .orElseThrow();

        var jwtToken = jwtService.generateToken(user);

        revokeAllUserTokens(user);
        saveUserToken(user, jwtToken);

        return AuthenticationResponseDto.builder()
                .token(jwtToken)
                .build();
    }

    @Override
    public void recoverPassword(String email) {
        Client client = clientRepository.findByEmail(email)
                .orElseThrow(() -> new BusinessException(ErrorMessages.getErrorMessage("USER_NOT_FOUND_BY_EMAIL")));

        EmailMessage emailMessage = EmailMessage.builder()
                .subject("Recuperação de senha")
                .body(EmailMessages.getRecoverPasswordMessage(client.getName(), encryptor.decrypt(client.getPasswordVerification())))
                .recipients(Collections.singleton(client.getEmail()))
                .build();

        emailService.sendEmail(emailMessage);
    }

    @Override
    public void changePassword(ChangePasswordRequestDto changePasswordRequestDto) {
        Client client = SecurityUtil.getClientFromJwt();

        validate(changePasswordRequestDto, client);
        client.setPassword(passwordEncoder.encode(changePasswordRequestDto.getNewPassword()));
        client.setPasswordVerification(encryptor.encrypt(changePasswordRequestDto.getNewPassword()));

        clientRepository.save(client);
    }

    private void validate(ChangePasswordRequestDto changePasswordRequestDto, Client client) {
        String actualPassword =  encryptor.decrypt(client.getPasswordVerification());

        if (!actualPassword.equals(changePasswordRequestDto.getOldPassword()))
            throw new BusinessException("Senha atual incorreta");

        String newPassword = changePasswordRequestDto.getNewPassword();
        String newPasswordVerify = changePasswordRequestDto.getNewPasswordVerify();

        if (!newPassword.equals(newPasswordVerify))
            throw new BusinessException("As senhas novas devem ser iguais");

    }

    private void saveUserToken(Client user, String jwtToken) {

        var token = Token.builder()
                .client(user)
                .token(jwtToken)
                .tokenType(TokenType.BEARER)
                .expired(false)
                .revoked(false)
                .build();

        tokenRepository.save(token);
    }

    private void revokeAllUserTokens(Client user) {

        var validUserTokens = tokenRepository.findAllValidTokenByUser(user.getId());

        if (validUserTokens.isEmpty()) {
            return;
        }

        validUserTokens.forEach(token -> {
            token.setExpired(true);
            token.setRevoked(true);
        });

        tokenRepository.saveAll(validUserTokens);
    }
}
