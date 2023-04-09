package com.coinsaver.services.authentication.interfaces;

import com.coinsaver.api.dtos.request.AuthenticationRequestDto;
import com.coinsaver.api.dtos.request.RegisterRequestDto;
import com.coinsaver.api.dtos.response.AuthenticationResponseDto;

public interface AuthenticationService {
    AuthenticationResponseDto register(RegisterRequestDto request);

    AuthenticationResponseDto authenticate(AuthenticationRequestDto request);

    void recoverPassword();
}
