package com.coinsaver.core.utils;

import com.coinsaver.domain.entities.Client;
import org.springframework.security.core.context.SecurityContextHolder;

public interface SecurityUtil {

    static Client getClientFromJwt() {
        return (Client) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    }
}
