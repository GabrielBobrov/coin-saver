package com.coinsaver.api.controllers;

import com.coinsaver.api.dtos.request.AuthenticationRequest;
import com.coinsaver.api.dtos.request.RegisterRequestDto;
import com.coinsaver.api.dtos.response.AuthenticationResponseDto;
import com.coinsaver.api.openapi.controller.AuthenticationControllerOpenApi;
import com.coinsaver.services.authentication.interfaces.AuthenticationService;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/auth")
@RequiredArgsConstructor
public class AuthenticationController implements AuthenticationControllerOpenApi {

    private final AuthenticationService authenticationService;

    @PostMapping("/register")
    @ResponseStatus(HttpStatus.CREATED)
    public AuthenticationResponseDto register(@RequestBody @Valid RegisterRequestDto request) {

        return authenticationService.register(request);
    }

    @PostMapping("/authenticate")
    @ResponseStatus(HttpStatus.CREATED)
    public AuthenticationResponseDto authenticate(@RequestBody @Valid AuthenticationRequest request) {

        return authenticationService.authenticate(request);
    }
}
