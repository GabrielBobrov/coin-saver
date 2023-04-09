package com.coinsaver.domain.entities;

import lombok.Builder;
import lombok.Getter;

import java.util.Set;

@Getter
@Builder
public class EmailMessage {

    private Set<String> recipients;
    private String subject;
    private String body;

}
