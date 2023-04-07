package com.coinsaver.domain.entities;

import lombok.Builder;
import lombok.Getter;

import java.util.Set;

@Getter
@Builder
public class EmailMessage {

    private Set<String> destinatarios;
    private String assunto;
    private String corpo;

}
