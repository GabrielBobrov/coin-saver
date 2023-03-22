package com.coinsaver.services.authentication.interfaces;

import com.coinsaver.api.dtos.request.AuthenticationRequest;
import com.coinsaver.api.dtos.request.RegisterRequest;
import com.coinsaver.api.dtos.response.AuthenticationResponse;

public interface AuthenticationService {
    AuthenticationResponse register(RegisterRequest request);
    AuthenticationResponse authenticate(AuthenticationRequest request);
}
