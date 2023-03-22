package com.coinsaver.api.controllers;

import com.coinsaver.api.dtos.request.AuthenticationRequest;
import com.coinsaver.api.dtos.request.RegisterRequest;
import com.coinsaver.api.dtos.response.AuthenticationResponse;
import com.coinsaver.services.authentication.interfaces.AuthenticationService;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/auth")
@RequiredArgsConstructor
public class AuthenticationController {

    private final AuthenticationService authenticationService;

    @PostMapping("/register")
    public AuthenticationResponse register(@RequestBody RegisterRequest request) {

        return authenticationService.register(request);
    }

    @PostMapping("/authenticate")
    public AuthenticationResponse authenticate(@RequestBody AuthenticationRequest request) {

        return authenticationService.authenticate(request);
    }


}
