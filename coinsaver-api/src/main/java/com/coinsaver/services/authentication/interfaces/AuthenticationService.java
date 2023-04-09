package com.coinsaver.services.authentication.interfaces;

import com.coinsaver.api.dtos.request.AuthenticationRequest;
import com.coinsaver.api.dtos.request.RegisterRequestDto;
import com.coinsaver.api.dtos.response.AuthenticationResponseDto;

public interface AuthenticationService {
    AuthenticationResponseDto register(RegisterRequestDto request);

    AuthenticationResponseDto authenticate(AuthenticationRequest request);

    void recoverPassword();
}
