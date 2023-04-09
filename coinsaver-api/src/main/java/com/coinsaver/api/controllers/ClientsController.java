package com.coinsaver.api.controllers;

import com.coinsaver.api.openapi.controller.ClientsControllerOpenApi;
import com.coinsaver.services.authentication.interfaces.AuthenticationService;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/clients")
@RequiredArgsConstructor
public class ClientsController implements ClientsControllerOpenApi {

    private final AuthenticationService authenticationService;

    @PostMapping("/recover-password")
    @ResponseStatus(HttpStatus.CREATED)
    public void recoverPassword() {

        authenticationService.recoverPassword();
    }
}
