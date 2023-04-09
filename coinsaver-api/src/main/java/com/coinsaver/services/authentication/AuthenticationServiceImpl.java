package com.coinsaver.services.authentication;


import com.coinsaver.api.dtos.request.AuthenticationRequest;
import com.coinsaver.api.dtos.request.RegisterRequestDto;
import com.coinsaver.api.dtos.response.AuthenticationResponseDto;
import com.coinsaver.core.enums.Role;
import com.coinsaver.core.enums.TokenType;
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
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.Set;

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

    public AuthenticationResponseDto register(RegisterRequestDto request) {
        var client = clientRepository.findByEmail(request.getEmail());

        if (client.isPresent())
            throw new BusinessException(ErrorMessages.getErrorMessage("USER_ALREADY_EXISTS"));

        var user = Client.builder()
                .name(request.getName())
                .email(request.getEmail())
                .password(encryptor.encrypt(request.getPassword()))
                .role(Role.USER)
                .build();

        var savedUser = clientRepository.save(user);
        var jwtToken = jwtService.generateToken(user);

        saveUserToken(savedUser, jwtToken);

        return AuthenticationResponseDto.builder()
                .token(jwtToken)
                .build();
    }

    public AuthenticationResponseDto authenticate(AuthenticationRequest request) {

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

        Set<String> set = new HashSet<>();
        set.add(client.getEmail());

        EmailMessage emailMessage = EmailMessage.builder()
                .subject("Recuperação de senha")
                .body(EmailMessages.getRecoverPasswordMessage(client.getName(), encryptor.decrypt(client.getPassword())))
                .recipients(set)
                .build();

        emailService.sendEmail(emailMessage);
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
